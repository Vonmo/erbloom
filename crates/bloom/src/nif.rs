use std::io::Write;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use rustler::resource::ResourceArc;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};

use atoms::{bindecode, binencode, error, ok, wrong_filter_type};
use container::SerializedFilter;
use filter::{BloomFilter, Filter, FilterType};
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
fn new<'a>(env: Env<'a>, opts: FilterOptions) -> NifResult<Term<'a>> {
    let filt = Filter::new(opts).map_err(|e| rustler::error::Error::Term(Box::new(e)))?;
    Ok((ok(), ResourceArc::new(FilterResource::from(filt))).encode(env))
}

#[rustler::nif]
fn ftype<'a>(env: Env<'a>, resource: ResourceArc<FilterResource>) -> NifResult<Term<'a>> {
    let filt_guard = resource.read();
    Ok((filt_guard.filter_type() as u32).encode(env))
}

#[rustler::nif(name = "serialize", schedule = "DirtyIo")]
fn serialize<'a>(env: Env<'a>, resource: ResourceArc<FilterResource>) -> NifResult<Term<'a>> {
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
fn deserialize<'a>(env: Env<'a>, serialized: LazyBinary<'a>) -> NifResult<Term<'a>> {
    match bincode::deserialize::<SerializedFilter>(&serialized) {
        Ok(f) => Ok((
            ok(),
            ResourceArc::new(FilterResource::from(Filter::restore(f).unwrap())),
        )
            .encode(env)),
        Err(_e) => Ok((error(), bindecode()).encode(env)),
    }
}

#[rustler::nif]
fn set<'a>(env: Env<'a>, resource: ResourceArc<FilterResource>, key: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let mut filt_guard = resource.write();
    match &mut *filt_guard {
        Filter::Forgetful(filt) => {
            let member = filt.set(&key);
            Ok(member.encode(env))
        }
        Filter::Bloom(filt) => {
            filt.set(&key);
            Ok(ok().encode(env))
        }
    }
}

#[rustler::nif]
fn vcheck<'a>(env: Env<'a>, resource: ResourceArc<FilterResource>, key: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let filt_guard = resource.read();
    Ok(filt_guard.check(&key).encode(env))
}

#[rustler::nif]
fn check_and_set<'a>(
    env: Env<'a>,
    resource: ResourceArc<FilterResource>,
    key: LazyBinary<'a>,
) -> NifResult<Term<'a>> {
    let mut filt_guard = resource.write();
    match &mut *filt_guard {
        Filter::Bloom(filter) => Ok(filter.check_and_set(&key).encode(env)),
        _ => Ok((error(), wrong_filter_type()).encode(env)),
    }
}

#[rustler::nif]
fn clear<'a>(env: Env<'a>, resource: ResourceArc<FilterResource>) -> NifResult<Term<'a>> {
    resource.write().clear();
    Ok(ok().encode(env))
}

// check a serialized bloom for key membership without fully deserializing the bloom
// specifically we want to avoid the very slow bitvec deserialization and simply compute
// the hash keys manually and check them inside the Erlang binary by hand
// for a 50mb bloom, this improves checking a serialized bloom from 25 seconds to 35 microseconds
#[rustler::nif]
fn check_serialized<'a>(env: Env<'a>, serialized: LazyBinary<'a>, key: LazyBinary<'a>) -> NifResult<Term<'a>> {
    match bincode::deserialize::<SerializedFilter>(&serialized) {
        Ok(f) => match f.opts.filter_type {
            Some(FilterType::Bloom) => {
                // TODO: The following block contains values
                // originally filled in by
                // `FilterOptions::default()`. Are these values
                // meaningful in any away? Is there a more elegent
                // solution to keep this `check_serialized()` working
                // correctly?
                //
                let opts = {
                    let mut opts = FilterOptions::default();
                    opts.filter_type = Some(FilterType::Bloom);
                    opts.items_count = Some(100);
                    opts.bitmap_size = Some(10);
                    opts
                };
                let filter = BloomFilter::new(opts).unwrap();
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

/// Represents either a borrowed `Binary` or `OwnedBinary`.
///
/// `LazyBinary` allows for the most efficient conversion from an
/// Erlang term to a byte slice. If the term is an actual Erlang
/// binary, constructing `LazyBinary` is essentially
/// zero-cost. However, if the term is any other Erlang type, it is
/// converted to an `OwnedBinary`, which requires a heap allocation.
enum LazyBinary<'a> {
    Owned(OwnedBinary),
    Borrowed(Binary<'a>),
}

impl<'a> std::ops::Deref for LazyBinary<'a> {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        match self {
            Self::Owned(owned) => owned.as_ref(),
            Self::Borrowed(borrowed) => borrowed.as_ref(),
        }
    }
}

impl<'a> rustler::Decoder<'a> for LazyBinary<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if term.is_binary() {
            Ok(Self::Borrowed(Binary::from_term(term)?))
        } else {
            Ok(Self::Owned(term.to_binary()))
        }
    }
}
