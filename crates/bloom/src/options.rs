use filter::FilterType;
use rustler::{Decoder, NifResult, Term};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Clone, Copy, Debug)]
pub struct FilterOptions {
    pub filter_type: Option<FilterType>,
    pub bitmap_size: Option<usize>,
    pub items_count: Option<usize>,
    pub capacity: Option<usize>,
    pub rotate_at: Option<usize>,
    pub fp_rate: Option<f64>,
}

impl Default for FilterOptions {
    fn default() -> FilterOptions {
        FilterOptions {
            filter_type: None,
            bitmap_size: None,
            items_count: None,
            capacity: None,
            rotate_at: None,
            fp_rate: None,
        }
    }
}

impl<'a> Decoder<'a> for FilterOptions {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let mut opts = Self::default();
        use rustler::{Error, MapIterator};
        for (key, value) in MapIterator::new(term).ok_or(Error::BadArg)? {
            match key.atom_to_string()?.as_ref() {
                "filter_type" => opts.filter_type = Some(value.decode()?),
                "bitmap_size" => opts.bitmap_size = Some(value.decode()?),
                "items_count" => opts.items_count = Some(value.decode()?),
                "capacity" => opts.capacity = Some(value.decode()?),
                "rotate_at" => opts.rotate_at = Some(value.decode()?),
                "fp_rate" => opts.fp_rate = Some(value.decode()?),
                _ => (),
            }
        }
        Ok(opts)
    }
}
