namespace Lazy

/// An interface for lazy.
type ILazy<'a> =
    /// Gets the value received by the supplier.
    abstract member Get: unit -> 'a