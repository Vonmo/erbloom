extern crate bloomfilter;
#[macro_use]
extern crate rustler;

use bloomfilter::Bloom;
use rustler::{Encoder, Env, NifResult, Term};
use rustler::resource::ResourceArc;
use std::sync::RwLock;

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

struct FilterResource {
    filter: RwLock<Bloom <Vec<u8>>>,
    bit_size: usize,
    items_count: usize,
}

rustler_export_nifs!(
    "bloom",
    [
        ("new", 2, new),
        ("serialize", 1, serialize),
        ("deserialize", 7, deserialize),
        ("set", 2, set),
        ("check", 2, check),
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
        filter: RwLock::new(
            Bloom::new(bitmap_size as usize, items_count as usize)
        ),
        bit_size: bitmap_size as usize,
        items_count: items_count as usize,
    });

    Ok((atoms::ok(), resource.encode(env)).encode(env))
}

fn serialize<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;

    let filter = resource.filter.read().unwrap();
    let sips = filter.sip_keys();

    Ok((atoms::ok(), (&filter.bitmap(),
                      filter.number_of_bits(),
                      filter.number_of_hash_functions(),
                      sips[0],
                      sips[1])).encode(env))
}

fn deserialize<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let bitmap: Vec<u8> = args[0].decode()?;
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
        bit_size: num_bits as usize,
        items_count: num_funs as usize,
    });

    Ok((atoms::ok(), resource.encode(env)).encode(env))
}

fn set<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;
    let key: Vec<u8> = args[1].decode()?;

    let mut filter = resource.filter.write().unwrap();
    (*filter).set(&key);

    Ok((atoms::ok(), resource.encode(env)).encode(env))
}

fn check<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;
    let key: Vec<u8> = args[1].decode()?;

    let filter = resource.filter.read().unwrap();

    Ok((atoms::ok(), filter.check(&key)).encode(env))
}

fn clear<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = args[0].decode()?;

    let new_resource = ResourceArc::new(FilterResource {
        filter: RwLock::new(
            Bloom::new(resource.bit_size, resource.items_count),
        ),
        bit_size: resource.bit_size,
        items_count: resource.items_count,
    });

    Ok((atoms::ok(), new_resource.encode(env)).encode(env))
}
