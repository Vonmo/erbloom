use std::io::Write;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use rustler::resource::ResourceArc;
use rustler::{Binary, Encoder, Env, MapIterator, NifResult, OwnedBinary, Term};

use atoms::{bindecode, binencode, error, ok, wrong_filter_type};
use filter::{BloomFilter, Filter, FilterType};
use container::SerializedFilter;
use options::FilterOptions;

// =================================================================================================
// resource
// =================================================================================================

#[repr(transparent)]
struct FilterResource(RwLock<Filter>);

impl FilterResource {
    fn read(&self) -> RwLockReadGuard<'_, Filter> {
        self.0.read().unwrap()
    }

    fn write(&self) -> RwLockWriteGuard<'_, Filter> {
        self.0.write().unwrap()
    }
}

impl From<Filter> for FilterResource {
    fn from(other: Filter) -> Self {
        FilterResource(RwLock::new(other))
    }
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(FilterResource, env);
    true
}

// =================================================================================================
// api
// =================================================================================================

#[rustler::nif]
fn new<'a>(env: Env<'a>, args: MapIterator) -> NifResult<Term<'a>> {
    let mut opts = FilterOptions::default();

    for (key, value) in args {
        let param = key.atom_to_string()?;
        match param.as_str() {
            "filter_type" => {
                opts.filter_type = match value.atom_to_string()?.as_str() {
                    "fbf" => FilterType::Forgetful,
                    _ => FilterType::Bloom,
                }
            }
            "bitmap_size" => {
                opts.bitmap_size = value.decode()?;
            }
            "items_count" => {
                opts.items_count = value.decode()?;
            }
            "capacity" => {
                opts.capacity = value.decode()?;
            }
            "rotate_at" => {
                opts.rotate_at = value.decode()?;
            }
            "fp_rate" => {
                opts.fp_rate = value.decode()?;
            }
            _ => {}
        }
    }

    let filt = Filter::new(opts);
    Ok((ok(), ResourceArc::new(FilterResource::from(filt))).encode(env))
}

#[rustler::nif]
fn ftype<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let filt_guard = resource.read();
    Ok((filt_guard.filter_type() as u32).encode(env))
}

#[rustler::nif(name = "serialize", schedule = "DirtyIo")]
fn serialize<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let serialized = resource.read().serialize();
    match serialized {
        Ok(bin_vec) => {
            let mut binary = OwnedBinary::new(bin_vec.len()).unwrap();
            binary.as_mut_slice().write_all(&bin_vec).unwrap();
            Ok((ok(), Binary::from_owned(binary, env)).encode(env))
        }
        Err(_e) => Ok((error(), binencode()).encode(env)),
    }
}

#[rustler::nif(name = "deserialize", schedule = "DirtyIo")]
fn deserialize<'a>(env: Env<'a>, serialized: Term<'a>) -> NifResult<Term<'a>> {
    let serialized: Binary = if serialized.is_binary() {
        serialized.decode()?
    } else {
        Binary::from_owned(serialized.to_binary(), env)
    };
    match bincode::deserialize::<SerializedFilter>(&serialized.as_slice()[..]) {
        Ok(f) => {
            Ok((ok(), ResourceArc::new(FilterResource::from(Filter::restore(f)))).encode(env))
        }
        Err(_e) => Ok((error(), bindecode()).encode(env)),
    }
}

#[rustler::nif]
fn set<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);
    let mut filt_guard = resource.write();
    match &mut *filt_guard {
        Filter::Forgetful(filt) => {
            let member = filt.set(&key);
            Ok(member.encode(env))
        },
        Filter::Bloom(filt) => {
            filt.set(&key);
            Ok(ok().encode(env))
        }
    }
}

#[rustler::nif]
fn vcheck<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);
    let filt_guard = resource.read();
    Ok(filt_guard.check(&key).encode(env))
}

#[rustler::nif]
fn check_and_set<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);
    let mut filt_guard = resource.write();
    match &mut *filt_guard {
        Filter::Bloom(filter) => Ok(filter.check_and_set(&key).encode(env)),
        _ => Ok((error(), wrong_filter_type()).encode(env)),
    }
}

#[rustler::nif]
fn clear<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    resource.write().clear();
    Ok(ok().encode(env))
}

// check a serialized bloom for key membership without fully deserializing the bloom
// specifically we want to avoid the very slow bitvec deserialization and simply compute
// the hash keys manually and check them inside the Erlang binary by hand
// for a 50mb bloom, this improves checking a serialized bloom from 25 seconds to 35 microseconds
#[rustler::nif]
fn check_serialized<'a>(env: Env<'a>, serialized: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let serialized: Binary = if serialized.is_binary() {
        serialized.decode()?
    } else {
        Binary::from_owned(serialized.to_binary(), env)
    };
    let key = key_to_bin(env, key);

    match bincode::deserialize::<SerializedFilter>(&serialized.as_slice()[..]) {
        Ok(f) => match f.opts.filter_type {
            FilterType::Bloom => {
                let filter = BloomFilter::new(FilterOptions::default());
                Ok((filter.check_serialized(f, &key)).encode(env))
            }
            _ => Ok((error(), wrong_filter_type()).encode(env)),
        },
        Err(_e) => Ok((error(), bindecode()).encode(env)),
    }
}

// =================================================================================================
// helpers
// =================================================================================================
fn key_to_bin<'a>(env: Env<'a>, key: Term<'a>) -> Vec<u8> {
    let bin = if key.is_binary() {
        Binary::from_term(key).unwrap()
    } else {
        Binary::from_owned(key.to_binary(), env)
    };
    bin.as_slice().to_vec()
}
