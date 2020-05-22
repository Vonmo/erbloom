use std::io::Write;
use std::sync::RwLock;

use rustler::{Binary, Encoder, Env, MapIterator, NifResult, OwnedBinary, Term};
use rustler::resource::ResourceArc;

use atoms::{bindecode, binencode, error, ok, wrong_filter_type};
use bloom::BloomFilter;
use container::{FilterContainer, FilterType, SerializedFilter};
use forgetful::ForgetfulFilter;
use options::FilterOptions;

// =================================================================================================
// resource
// =================================================================================================

struct FilterResource {
    current_type: FilterType,
    bloom: RwLock<FilterContainer<BloomFilter>>,
    forgetful: RwLock<FilterContainer<ForgetfulFilter>>,
}

impl Default for FilterResource {
    fn default() -> FilterResource {
        FilterResource {
            current_type: FilterType::Bloom,
            bloom: RwLock::new(FilterContainer::new(BloomFilter::new(FilterOptions::default()))),
            forgetful: RwLock::new(FilterContainer::new(ForgetfulFilter::new(FilterOptions::default()))),
        }
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
                    _ => FilterType::Bloom
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

    let mut resource = FilterResource::default();

    match opts.filter_type {
        FilterType::Forgetful => {
            resource.forgetful = RwLock::new(FilterContainer::new(ForgetfulFilter::new(opts)));
            resource.current_type = FilterType::Forgetful;
        }
        FilterType::Bloom => {
            resource.bloom = RwLock::new(FilterContainer::new(BloomFilter::new(opts)));
            resource.current_type = FilterType::Bloom;
        }
    };

    Ok((ok(), ResourceArc::new(resource)).encode(env))
}

#[rustler::nif]
fn ftype<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    Ok((resource.current_type as u32).encode(env))
}

#[rustler::nif(name = "serialize", schedule = "DirtyIo")]
fn serialize<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let serialized = match resource.current_type {
        FilterType::Forgetful => {
            resource.forgetful.read().unwrap().inner.serialize()
        }
        FilterType::Bloom => {
            resource.bloom.read().unwrap().inner.serialize()
        }
    };
    match serialized {
        Ok(bin_vec) => {
            let mut binary = OwnedBinary::new(bin_vec.len()).unwrap();
            binary.as_mut_slice().write_all(&bin_vec).unwrap();
            Ok((ok(), Binary::from_owned(binary, env)).encode(env))
        }
        Err(_e) => {
            Ok((error(), binencode()).encode(env))
        }
    }
}

#[rustler::nif(name = "deserialize", schedule = "DirtyIo")]
fn deserialize<'a>(env: Env<'a>, serialized: Term<'a>) -> NifResult<Term<'a>> {
    let serialized: Binary = if serialized.is_binary() {
        serialized.decode()?
    } else {
        Binary::from_owned(serialized.to_binary(), env)
    };
    let mut resource = FilterResource::default();
    match bincode::deserialize::<SerializedFilter>(&serialized.as_slice()[..]) {
        Ok(f) => {
            match f.opts.filter_type {
                FilterType::Forgetful => {
                    resource.forgetful = RwLock::new(FilterContainer::new(ForgetfulFilter::restore(f)));
                    resource.current_type = FilterType::Forgetful;
                }
                FilterType::Bloom => {
                    resource.bloom = RwLock::new(FilterContainer::new(BloomFilter::restore(f)));
                    resource.current_type = FilterType::Bloom;
                }
            }
            Ok((ok(), ResourceArc::new(resource)).encode(env))
        }
        Err(_e) => {
            Ok((error(), bindecode()).encode(env))
        }
    }
}


#[rustler::nif]
fn set<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);

    match resource.current_type {
        FilterType::Forgetful => {
            let filter = &mut resource.forgetful.write().unwrap().inner;
            let member = filter.set(&key);

            Ok(member.encode(env))
        }
        FilterType::Bloom => {
            resource.bloom.write().unwrap().inner.set(&key);
            Ok(ok().encode(env))
        }
    }
}

#[rustler::nif]
fn vcheck<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);

    match resource.current_type {
        FilterType::Forgetful => {
            let filter = &mut resource.forgetful.write().unwrap().inner;
            Ok(filter.check(&key.as_slice().to_vec()).encode(env))
        }
        FilterType::Bloom => {
            Ok(resource.bloom.read().unwrap().inner.check(&key.as_slice().to_vec()).encode(env))
        }
    }
}

#[rustler::nif]
fn check_and_set<'a>(env: Env<'a>, filter_ref: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;
    let key = key_to_bin(env, key);

    match resource.current_type {
        FilterType::Bloom => Ok(resource.bloom.write().unwrap().inner.check_and_set(&key.as_slice().to_vec()).encode(env)),
        _ => Ok((error(), wrong_filter_type()).encode(env)),
    }
}

#[rustler::nif]
fn clear<'a>(env: Env<'a>, filter_ref: Term<'a>) -> NifResult<Term<'a>> {
    let resource: ResourceArc<FilterResource> = filter_ref.decode()?;

    match resource.current_type {
        FilterType::Forgetful => {
            resource.forgetful.write().unwrap().inner.clear();
        }
        FilterType::Bloom => {
            resource.bloom.write().unwrap().inner.clear();
        }
    }

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

    let resource = FilterResource::default();
    match bincode::deserialize::<SerializedFilter>(&serialized.as_slice()[..]) {
        Ok(f) => {
            match f.opts.filter_type {
                FilterType::Bloom => {
                    Ok((resource.bloom.read().unwrap().inner.check_serialized(f, &key)).encode(env))
                }
                _ => {
                    Ok((error(), wrong_filter_type()).encode(env))
                }
            }
        }
        Err(_e) => {
            Ok((error(), bindecode()).encode(env))
        }
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