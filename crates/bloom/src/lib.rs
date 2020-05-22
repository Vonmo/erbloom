extern crate bincode;
extern crate bloomfilter;
extern crate core;
extern crate rustler;
extern crate serde;
extern crate siphasher;

mod atoms;
mod options;
mod container;
mod bloom;
mod forgetful;
mod nif;

rustler::init!(
    "bloom_nif",
    [
        nif::new,
        nif::serialize,
        nif::deserialize,
        nif::set,
        nif::vcheck,
        nif::clear,
        nif::check_and_set,
        nif::ftype,
        nif::check_serialized,
    ],
    load = nif::on_load
);
