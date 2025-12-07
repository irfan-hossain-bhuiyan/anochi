use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{MaybeUninit};
use std::ops::{Deref, Index, IndexMut};

use enum_dispatch::enum_dispatch;
use macros::generate_unchecked;
pub struct ZeroSizePopError;
/// Type alias for hash values - can be easily changed to u128 or larger in the future
pub type HashValue = u64;

/// Hash Consing Container - A generic data structure that uses hash-based memoization
/// to store objects and detect duplicates. Each push operation returns a hash that
/// serves as a pointer to the object, enabling O(1) access and deduplication.
/// Type-safe hash pointer for HashCons
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashPtr<T: ?Sized> {
    hash: HashValue,
    _marker: std::marker::PhantomData<T>,
}

impl<T> HashPtr<T> {
    pub fn as_hash_value(&self) -> HashValue {
        self.hash
    }
}
impl<T: ?Sized> Clone for HashPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized> Copy for HashPtr<T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FreeRange {
    start: usize,
    end: usize,
}

impl FreeRange {
    fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    
    fn contains(&self, index: usize) -> bool {
        index >= self.start && index <= self.end
    }
    
    fn len(&self) -> usize {
        self.end - self.start + 1
    }
}

impl PartialOrd for FreeRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FreeRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

#[derive(Debug,)]
pub struct IndexPtr<T> {
    index: usize,
    _marker: std::marker::PhantomData<T>,
}
impl<T> Clone for IndexPtr<T>{
    fn clone(&self) -> Self { *self }
}
impl<T> Copy for IndexPtr<T> {}
impl<T> PartialEq for IndexPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index==other.index
    }
}

impl<T> IndexPtr<T> {
    pub fn as_index(&self) -> usize {
        self.index
    }
}
pub trait Allocator<T>{
    fn get_from_ptr_checked(&self,ptr:IndexPtr<T>)->Option<&T>;
    fn get_mut_from_ptr_checked(&mut self,ptr:IndexPtr<T>)->Option<&mut T>;
    fn get_from_ptr(&self,ptr:IndexPtr<T>)->&T {
        self.get_from_ptr_checked(ptr).unwrap()
    }
    fn get_mut_from_ptr(&mut self,ptr:IndexPtr<T>)->&mut T {
        self.get_mut_from_ptr_checked(ptr).unwrap()
    }
}
impl<T> Allocator<T> for IndexCons<T>{
    fn get_from_ptr_checked(&self,ptr:IndexPtr<T>)->Option<&T> {
        self.get_checked(ptr)
    }
    fn get_mut_from_ptr_checked(&mut self,ptr:IndexPtr<T>)->Option<&mut T> {
        self.get_mut_checked(ptr)
    }
}
#[derive(Debug, Clone)]
pub struct IndexCons<T> {
    storage: Vec<Option<T>>,
    free_ranges: BTreeSet<FreeRange>,
}

impl<T> IndexCons<T> {
    pub fn new() -> Self {
        IndexCons {
            storage: Vec::new(),
            free_ranges: BTreeSet::new(),
        }
    }

    pub fn push(&mut self, obj: T) -> IndexPtr<T> {
        if let Some(range) = self.free_ranges.iter().next().cloned() {
            let index = range.start;
            self.storage[index] = Some(obj);
            
            self.free_ranges.remove(&range);
            if range.start < range.end {
                let new_range = FreeRange::new(index + 1, range.end);
                self.free_ranges.insert(new_range);
            }
            
            IndexPtr {
                index,
                _marker: std::marker::PhantomData,
            }
        } else {
            self.storage.push(Some(obj));
            IndexPtr {
                index: self.storage.len() - 1,
                _marker: std::marker::PhantomData,
            }
        }
    }

    pub fn remove(&mut self, index_ptr: &IndexPtr<T>) -> Option<T> {
        let index = index_ptr.index;
        let value = self.storage.get_mut(index)?.take()?;
        
        let prev_range = if index > 0 {
            let search = FreeRange::new(index - 1, index - 1);
            self.free_ranges.range(..=search).next_back().cloned()
        } else {
            None
        };
        
        let next_range = {
            let search = FreeRange::new(index + 1, index + 1);
            self.free_ranges.range(search..).next().cloned()
        };
        
        let mut merged_start = index;
        let mut merged_end = index;
        
        if let Some(prev) = prev_range {
            if prev.end == index - 1 {
                merged_start = prev.start;
                self.free_ranges.remove(&prev);
            }
        }
        
        if let Some(next) = next_range {
            if next.start == index + 1 {
                merged_end = next.end;
                self.free_ranges.remove(&next);
            }
        }
        
        self.free_ranges.insert(FreeRange::new(merged_start, merged_end));
        Some(value)
    }
    #[generate_unchecked]
    pub fn get_checked(&self, index_ptr: IndexPtr<T>) -> Option<&T> {
        self.storage.get(index_ptr.index)?.as_ref()
    }
    #[generate_unchecked]
    pub fn get_mut_checked(&mut self, index_ptr: IndexPtr<T>) -> Option<&mut T> {
        self.storage.get_mut(index_ptr.index)?.as_mut()
    }

    pub fn len(&self) -> usize {
        self.storage.len() - self.free_ranges.iter().map(|r| r.len()).sum::<usize>()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&mut self) {
        self.storage.clear();
        self.free_ranges.clear();
    }
}

impl<T> Default for IndexCons<T> {
    fn default() -> Self {
        Self::new()
    }
}

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
pub trait Mappable<NOW, AFTER> {
    type Mapped;

    fn inner_map<F>(self, f: &mut F) -> Self::Mapped
    where
        F: FnMut(NOW) -> AFTER;
    //TODO: Tell the compiler to fix this mappable issue.
    //fn trymap<F>(self,f:F) ->Option<Self::Mapped>
    //    where
    //        F: FnMut(NOW) -> Option<AFTER>{
    //    self.map(f.)
    //}
}

impl<T, U> Mappable<T, U> for Vec<T> {
    type Mapped = Vec<U>;
    fn inner_map<F>(self, f: &mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        self.into_iter().map(f).collect()
    }
}
impl<K: Ord, T, U> Mappable<T, U> for BTreeMap<K, T> {
    fn inner_map<F>(self, f: &mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        self.into_iter().map(|(k, d)| (k, f(d))).collect()
    }

    type Mapped = BTreeMap<K, U>;
}

impl<T, U: Ord> Mappable<T, U> for BTreeSet<T> {
    type Mapped = BTreeSet<U>;
    fn inner_map<F>(self, f: &mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        self.into_iter().map(f).collect()
    }
}
impl<K: Hash + Eq, T, U> Mappable<T, U> for HashMap<K, T> {
    type Mapped = HashMap<K, U>;
    fn inner_map<F>(self, f: &mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        self.into_iter().map(|(key, data)| (key, f(data))).collect()
    }
}
#[enum_dispatch]
pub trait CodeError {
    fn err_str(code: &str) -> String;
}
pub struct SizedArray<T, const MAX_SIZE: usize = 1024> {
    array: [T; MAX_SIZE],
    size: usize,
}

impl<T, const MAX_SIZE: usize> Allocator<T> for SizedArray<T, MAX_SIZE> {
    fn get_from_ptr_checked(&self,ptr:IndexPtr<T>)->Option<&T> {
        self.get_checked(ptr.index)
    }
    fn get_mut_from_ptr_checked(&mut self,ptr:IndexPtr<T>)->Option<&mut T> {
        self.get_mut_checked(ptr.index)
    }
}


impl<T:Debug, const MAX_SIZE: usize> Debug for SizedArray<T, MAX_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.array[0..self.size],f)
    }
}

impl<T, const MAX_SIZE: usize> Deref for SizedArray<T, MAX_SIZE> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.array[0..self.size]
    }
}


impl<T, const MAX_SIZE: usize> IndexMut<usize> for SizedArray<T, MAX_SIZE> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T, const MAX_SIZE: usize> Index<usize> for SizedArray<T, MAX_SIZE> {
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
    }
    type Output=T;
}

impl<T, const MAX_SIZE: usize> Default for SizedArray<T, MAX_SIZE> {
    fn default() -> Self {
        unsafe {
            Self {
                array: MaybeUninit::uninit().assume_init(),
                size: 0,
            }
        }
    }
}

impl<T, const MAX_SIZE: usize> SizedArray<T, MAX_SIZE> {
    pub fn push_back(&mut self, value: T) -> IndexPtr<T> {
        let index = self.size;
        self.array[self.size] = value;
        self.size += 1;
        IndexPtr {
            index,
            _marker: PhantomData,
        }
    }
    #[generate_unchecked]
    fn get_checked(&self, index: usize) -> Option<&T> {
        if index < self.size {
            return Some(&self.array[index]);
        }
        None
    }
    #[generate_unchecked]
    fn get_mut_checked(&mut self, index: usize) -> Option<&mut T> {
        if index < self.size {
            return Some(&mut self.array[index]);
        }
        None
    }
    #[generate_unchecked]
    fn pop_checked(&mut self)->Result<(),&'static str>{
        if self.is_empty(){Err("You poped the array when size is 0")}
        else {
            self.size-=1;
            Ok(())
        }
    }
    pub fn shrink_size(&mut self,new_size:usize)->Result<(),&'static str>{
        if self.size<new_size{Err("You resized the array to grow")}
        else {
            self.size=new_size;
            Ok(())
        }
    }

    fn is_empty(&self) -> bool {
        self.size==0
    }

    pub(crate) fn size(&self) -> usize {
        self.size
    }
}


