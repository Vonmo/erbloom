extern crate bloomfilter;
#[macro_use]
extern crate rustler;
extern crate siphasher;

use bloomfilter::Bloom;
use rustler::resource::ResourceArc;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};
use siphasher::sip::SipHasher13;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::sync::RwLock;

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

struct FilterResource {
    filter: RwLock<Bloom<[u8]>>,
}

rustler_export_nifs!(
    "bloom",
    [
        ("new", 2, new),
        ("new_for_fp_rate", 2, new_for_fp_rate),
        ("serialize", 1, serialize),
        ("deserialize", 7, deserialize),
        ("set", 2, set),
        ("check_nif", 2, check),
        ("check_nif", 8, check_ro),
        ("check_and_set", 2, check_and_set),
        ("clear", 1, clear),
    ],
    Some(on_load)
);

fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(FilterResource, env);
    true
}

fn new<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let bitmap_size: i64 = args[0].decode()?;
    let items_count: i64 = args[1].decode()?;

    let resource = ResourceArc::new(FilterResource {
        filter: RwLock::new(Bloom::new(bitmap_size as usize, items_count as usize)),
    });

    Ok((atoms::ok(), resource.encode(env)).encode(env))
}

fn new_for_fp_rate<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let items_count: i64 = args[0].decode()?;
    let fp_p: f64 = args[1].decode()?;

    let resource = ResourceArc::new(FilterResource {
        filter: RwLock::new(Bloom::new_for_fp_rate(items_count as usize, fp_p)),
    });

    Ok((atoms::ok(), resource).encode(env))
}

fn serialize<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;

    let filter = resource.filter.read().unwrap();
    let sips = filter.sip_keys();
    let bitmap = filter.bitmap();
    let mut binary = OwnedBinary::new(bitmap.len()).unwrap();
    binary.as_mut_slice().write_all(&bitmap).unwrap();

    Ok((
        atoms::ok(),
        (
            Binary::from_owned(binary, env),
            filter.number_of_bits(),
            filter.number_of_hash_functions(),
            sips[0],
            sips[1],
        ),
    )
        .encode(env))
}

fn deserialize<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let bitmap: Binary = args[0].decode()?;
    let num_bits: u64 = args[1].decode()?;
    let num_funs: u32 = args[2].decode()?;
    let sip00: u64 = args[3].decode()?;
    let sip01: u64 = args[4].decode()?;
    let sip10: u64 = args[5].decode()?;
    let sip11: u64 = args[6].decode()?;

    let resource = ResourceArc::new(FilterResource {
        filter: RwLock::new(Bloom::from_existing(
            &bitmap,
            num_bits,
            num_funs,
            [(sip00, sip01), (sip10, sip11)],
        )),
    });

    Ok((atoms::ok(), resource).encode(env))
}

fn set<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;
    let key: Binary = if args[1].is_binary() {
        args[1].decode()?
    } else {
        Binary::from_owned(args[1].to_binary(), env)
    };

    let mut filter = resource.filter.write().unwrap();
    (*filter).set(&key);

    Ok(atoms::ok().encode(env))
}

fn check<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;
    let key: Binary = if args[1].is_binary() {
        args[1].decode()?
    } else {
        Binary::from_owned(args[1].to_binary(), env)
    };

    let filter = resource.filter.read().unwrap();

    Ok(filter.check(&key).encode(env))
}

// check a serialized bloom for key membership without fully deserializing the bloom
// specifically we want to avoid the very slow bitvec deserialization and simply compute
// the hash keys manually and check them inside the Erlang binary by hand
// for a 50mb bloom, this improves checking a serialized bloom from 25 seconds to 35 microseconds
fn check_ro<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let bitmap: Binary = args[0].decode()?;
    let num_bits: u64 = args[1].decode()?;
    let num_funs: u32 = args[2].decode()?;
    let sip00: u64 = args[3].decode()?;
    let sip01: u64 = args[4].decode()?;
    let sip10: u64 = args[5].decode()?;
    let sip11: u64 = args[6].decode()?;
    let key: Binary = if args[7].is_binary() {
        args[7].decode()?
    } else {
        Binary::from_owned(args[7].to_binary(), env)
    };

    let sips = [
        SipHasher13::new_with_keys(sip00, sip01),
        SipHasher13::new_with_keys(sip10, sip11),
    ];

    let mut hashes = [0u64, 0u64];
    for k_i in 0..num_funs {
        let bit_offset = (bloom_hash(&mut hashes, &key, k_i, &sips) % num_bits) as usize;
        let byte_offset = bit_offset / 8;
        let bit = 7 - (bit_offset % 8);
        if (bitmap[byte_offset] >> bit) & 1 != 1 {
            return Ok(false.encode(env));
        }
    }
    Ok(true.encode(env))
}

// helper for check_ro, extracted from the bloom crate source code
fn bloom_hash(hashes: &mut [u64; 2], item: &[u8], k_i: u32, sips: &[SipHasher13; 2]) -> u64 {
    if k_i < 2 {
        let mut sip = sips[k_i as usize];
        item.hash(&mut sip);
        let hash = sip.finish();
        hashes[k_i as usize] = hash;
        hash
    } else {
        hashes[0].wrapping_add((u64::from(k_i)).wrapping_mul(hashes[1]) % 0xffff_ffff_ffff_ffc5)
    }
}

fn check_and_set<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;
    let key: Binary = if args[1].is_binary() {
        args[1].decode()?
    } else {
        Binary::from_owned(args[1].to_binary(), env)
    };

    let mut filter = resource.filter.write().unwrap();

    Ok(filter.check_and_set(&key).encode(env))
}

fn clear<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;

    let mut filter = resource.filter.write().unwrap();
    (*filter).clear();

    Ok(atoms::ok().encode(env))
}
