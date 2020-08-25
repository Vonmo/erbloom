use std::error;

use bloomfilter::Bloom;

use container::{RawSerializedFilter, SerializedFilter};
use options::FilterOptions;
use super::FilterType;

type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

pub struct ForgetfulFilter {
    pub filters: Vec<Bloom<[u8]>>,
    pub bitmap_size: usize,
    pub items_count: usize,
    pub capacity: usize,
    pub rotate_at: usize,
    pub insertion_count: usize,
    pub fp_rate: f64,
}

impl ForgetfulFilter {
    pub fn new(opts: FilterOptions) -> ForgetfulFilter {
        let mut forgetfulfilter = ForgetfulFilter {
            filters: Vec::with_capacity(opts.capacity),
            bitmap_size: opts.bitmap_size,
            items_count: opts.items_count,
            capacity: opts.capacity,
            rotate_at: opts.rotate_at,
            fp_rate: opts.fp_rate,
            insertion_count: 0,
        };

        for _x in 0..opts.capacity {
            forgetfulfilter
                .filters
                .push(if opts.items_count > 0 && opts.bitmap_size == 0 {
                    Bloom::new_for_fp_rate(opts.items_count as usize, opts.fp_rate)
                } else {
                    Bloom::new(opts.bitmap_size, opts.items_count)
                })
        }

        forgetfulfilter
    }

    pub fn set(&mut self, key: &[u8]) -> bool {
        let num_inner_filters = self.filters.len();

        // check membership
        let mut member = false;
        // check the overlapping blooms 2 by 2
        for x in 0..num_inner_filters - 2 {
            if self.filters[x].check(key) && self.filters[x + 1].check(key) {
                member = true;
                break;
            }
        }

        if !member {
            // check last bloom
            member = self.filters[num_inner_filters - 1].check(key);
        }
        if !member {
            self.insertion_count += 1;
            if self.insertion_count >= self.rotate_at {
                self.insertion_count = 0;
                // rotate the oldest bloom to the start of the list
                // and clear it
                self.filters.rotate_right(1);
                self.filters[0].clear();
            }
            // set in the future and current
            self.filters[0].set(key);
            self.filters[1].set(key);
        }

        member
    }

    pub fn check(&self, key: &[u8]) -> bool {
        let num_inner_filters = self.filters.len();
        // check the overlapping blooms 2 by 2
        for x in 0..num_inner_filters - 2 {
            if self.filters[x].check(&key) && self.filters[x + 1].check(&key) {
                return true;
            }
        }
        // check last bloom
        self.filters[num_inner_filters - 1].check(&key)
    }

    pub fn clear(&mut self) {
        let num_inner_filters = self.filters.len();
        for x in 0..num_inner_filters - 2 {
            self.filters[x].clear()
        };
    }

    pub fn serialize(&self) -> Result<Vec<u8>> {
        let mut opts = FilterOptions::default();
        opts.filter_type = FilterType::Forgetful;
        opts.bitmap_size = self.bitmap_size;
        opts.items_count = self.items_count;
        opts.capacity = self.capacity;
        opts.rotate_at = self.rotate_at;
        opts.fp_rate = self.fp_rate;

        let mut serialized_filters: Vec<RawSerializedFilter> = vec![];
        for i in 0..opts.capacity {
            let filter = &self.filters[i];
            let sips = filter.sip_keys();
            let bitmap = filter.bitmap();
            serialized_filters.push(RawSerializedFilter {
                payload: bitmap,
                num_bits: filter.number_of_bits(),
                num_funs: filter.number_of_hash_functions(),
                sip00: sips[0].0,
                sip01: sips[0].1,
                sip10: sips[1].0,
                sip11: sips[1].1,
            })
        }
        match bincode::serialize(&SerializedFilter {
            filters: serialized_filters,
            opts: opts,
            upsert_num: self.insertion_count as u64,
        }) {
            Ok(res) => Ok(res),
            Err(e) => Err(e)
        }
    }

    pub fn restore(prev_filter: SerializedFilter) -> ForgetfulFilter {
        let mut filters: Vec<Bloom<[u8]>> = vec![];

        for i in 0..prev_filter.opts.capacity {
            let pf = &prev_filter.filters.to_vec()[i];
            filters.push(Bloom::from_existing(
                &pf.payload,
                pf.num_bits,
                pf.num_funs,
                [(pf.sip00, pf.sip01), (pf.sip10, pf.sip11)],
            ))
        }

        ForgetfulFilter {
            filters: filters,
            bitmap_size: prev_filter.opts.bitmap_size,
            items_count: prev_filter.opts.items_count,
            capacity: prev_filter.opts.capacity,
            rotate_at: prev_filter.opts.rotate_at,
            fp_rate: prev_filter.opts.fp_rate,
            insertion_count: prev_filter.upsert_num as usize,
        }
    }
}
