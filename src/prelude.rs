use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::mem::size_of;

/// Type alias for hash values - can be easily changed to u128 or larger in the future
pub type HashValue = u64;

fn slice_index_from_vec<T>(vec: &[T], slice: &[T]) -> Option<usize> {
    let vec_start = vec.as_ptr() as usize;
    let slice_start = slice.as_ptr() as usize;

    // Check if slice is within vec
    if slice_start >= vec_start && slice_start <= vec_start + std::mem::size_of_val(vec) {
        let offset_bytes = slice_start - vec_start;
        Some(offset_bytes / size_of::<T>())
    } else {
        None
    }
}

/// Hash Consing Container - A generic data structure that uses hash-based memoization
/// to store objects and detect duplicates. Each push operation returns a hash that
/// serves as a pointer to the object, enabling O(1) access and deduplication.
/// Type-safe hash pointer for HashCons
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashPtr<T:?Sized> {
    hash: HashValue,
    _marker: std::marker::PhantomData<T>,
}

impl<T> HashPtr<T> {
    pub fn as_hash_value(&self) -> HashValue {
        self.hash
    }
}
impl<T:?Sized> Clone for HashPtr<T>{
    fn clone(&self) -> Self { *self }
}
impl<T:?Sized> Copy for HashPtr<T>{}

#[derive(Debug, Clone)]
pub struct HashCons<T: Eq + Hash + Clone> {
    /// Maps hash values to their corresponding objects
    storage: BTreeMap<HashValue, T>,
}

impl<T: Eq + Hash + Clone> HashCons<T> {
    /// Creates a new empty HashCons container
    pub fn new() -> Self {
        HashCons {
            storage: BTreeMap::new(),
        }
    }

    /// Pushes an object into the container and returns its hash.
    /// If the object already exists, returns the existing hash (deduplication).
    pub fn push(&mut self, obj: T) -> HashPtr<T> {
        let mut hasher = DefaultHasher::new();
        obj.hash(&mut hasher);
        let hash = hasher.finish();

        self.storage.entry(hash).or_insert(obj);

        HashPtr {
            hash,
            _marker: std::marker::PhantomData,
        }
    }

    /// Retrieves an object by its hash. Returns None if not found.
    pub fn get(&self, hash_ptr: &HashPtr<T>) -> Option<&T> {
        self.storage.get(&hash_ptr.hash)
    }

    /// Retrieves a mutable reference to an object by its hash.
    pub fn get_mut(&mut self, hash_ptr: &HashPtr<T>) -> Option<&mut T> {
        self.storage.get_mut(&hash_ptr.hash)
    }

    /// Checks if an object with the given hash exists in the container
    pub fn contains(&self, hash_ptr: &HashPtr<T>) -> bool {
        self.storage.contains_key(&hash_ptr.hash)
    }

    /// Returns the number of unique objects in the container
    pub fn len(&self) -> usize {
        self.storage.len()
    }

    /// Checks if the container is empty
    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }

    /// Clears all objects from the container
    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

impl<T: Eq + Hash + Clone> Default for HashCons<T> {
    fn default() -> Self {
        Self::new()
    }
}
pub trait Mappable<NOW,AFTER>{
    type Mapped;

    fn inner_map<F>(self, f: F) -> Self::Mapped
    where
        F: FnMut(NOW) -> AFTER;
    //TODO: Tell the compiler to fix this mappable issue.
    //fn trymap<F>(self,f:F) ->Option<Self::Mapped>
    //    where
    //        F: FnMut(NOW) -> Option<AFTER>{
    //    self.map(f.)
    //}

}

impl<T,U> Mappable<T,U> for Vec<T>{
    type Mapped = Vec<U>;
    fn inner_map<F>(self, f: F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        self.into_iter().map(f).collect()
    }
}
impl<K:Ord,T,U> Mappable<T,U> for BTreeMap<K,T>{
    fn inner_map<F>(self,mut f: F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        self.into_iter().map(|(k,d)|(k,f(d))).collect()
    }

    type Mapped=BTreeMap<K,U>;
}


impl<T,U: Ord> Mappable<T,U> for BTreeSet<T>{
    type Mapped = BTreeSet<U>;
    fn inner_map<F>(self, f: F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        self.into_iter().map(f).collect()
    }
}
impl<K: Hash + Eq,T,U> Mappable<T,U> for HashMap<K,T>{
    type Mapped = HashMap<K,U>;
    fn inner_map<F>(self, mut f: F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        self.into_iter().map(|(key,data)|(key,f(data))).collect()
    }
}
