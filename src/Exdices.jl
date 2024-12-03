"""
This is an almost clean-slate re-write of InvertedIndices.jl, aiming to be faster.
"""
module Exdices

export Not

"""
    Not(i)

Using this within indexing should exclude the index `i`.

* For `i::Integer`, and `i::AbstractArray{<:Integer}`, this is like using `setdiff(begin:end, index)`, but more efficient.
  (Most efficient for integers and `i::UnitRange`.)
* For `i::AbstractArray{Bool}`, this is like `.!i`, but marginally more efficient.
* You can use `i::Colon` although that's just empty, like `1:0`.
* When indexing a NamedTuple, you can use `i::Symbol`, or several symbols.
* `Not(i,j,k)` means `Not([i,j,k])`.
* Some other iterators are handled... `Not([]) === Not(()) === Colon()`.
* At present `i::CartesianIndex` and `i::AbstractArray{<:CartesianIndex}` are not handled.

# Examples

```jldoctest
julia> mat = reshape(10:10:120, 3, 4)
3×4 reshape(::StepRange{Int64, Int64}, 3, 4) with eltype Int64:
 10  40  70  100
 20  50  80  110
 30  60  90  120

julia> mat[Not(2), :]  # == mat[[1, 3], :]
2×4 Matrix{Int64}:
 10  40  70  100
 30  60  90  120

julia> view(mat, :, Not(2:3))  # == mat[:, setdiff(begin:end, 2:3)]
3×2 view(reshape(::StepRange{Int64, Int64}, 3, 4), :, [1, 4]) with eltype Int64:
 10  100
 20  110
 30  120

julia> @view mat[Not(iseven.(mat ./ 10))]  # == mat[findall(isodd, vec(mat) ./ 10)]
6-element view(::StepRange{Int64, Int64}, [1, 3, 5, 7, 9, 11]) with eltype Int64:
  10
  30
  50
  70
  90
 110

julia> alphabet = (a=1, b="two", c=3.0, d=:IV);

julia> alphabet[Not(:b, :d)]
(a = 1, c = 3.0)

julia> alphabet[Not(begin:2)]
(c = 3.0, d = :IV)
```
"""
struct Not{S}
    skip::S
    # Not(::Nothing) = new{Nothing}(nothing)
    Not(::Colon) = new{Colon}(:)
    Not(index::Integer) = new{Int}(nonBool2Int(index))
    # Not(index::CartesianIndex) = new{typeof(index)}(index)
    function Not(index::AbstractArray{<:Integer})
        sorted = uniquesort(index)  # do this once, on construction?
        new{typeof(sorted)}(sorted)
    end
    Not(labels::Symbol...) = new{typeof(labels)}(labels)
end

# The case Not([]) was tested. Let's allow such things, but make them tight on construction:
function Not(iter)
    isempty(iter) && return Colon()
    all(x -> x isa Integer, iter) && Not(vec(collect(Int, iter)))  # can this ever fail? Maybe it should make Vector{Int} explicitly.
    all(x -> x isa Symbol, iter) && return Not(iter...)
    throw(ArgumentError("invalid indices"))
end
# function Not(hmm::AbstractArray)
#     isempty(hmm) && return Not(nothing)  # this could also just be Colon()
#     better = map(identity, hmm)
#     eltype(better) <: Integer && return Not(better)
#     eltype(better) <: Symbol && return Not(better...)
#     throw(ArgumentError("invalid indices"))
# end
# Not(()) = Not(nothing)  # this could also just be Colon()
# Not(labels::Tuple{Symbol, Vararg}) = ...

nonBool2Int(x::Bool) = throw(ArgumentError(lazy"invalid index $x of type Bool"))
nonBool2Int(x::Integer) = Int(x)

uniquesort(ind::AbstractArray) = uniquesort(vec(ind))
uniquesort(ind::AbstractVector{<:Integer}) = issorted(ind, lt=(<=)) ? ind : unique!(sort(ind))  # maybe should make Vector?
uniquesort(mask::AbstractVector{Bool}) = mask
uniquesort(r::AbstractRange) = step(r) > 0 ? r : step(r) == 0 ? r[end:end] : reverse(r)
uniquesort(r::AbstractUnitRange) = r

function Not(i::Integer, jk::Integer...)
    # any(Base.Fix2(isa, Bool), (i, jk...)) && throw(ArgumentError("can't accept Bool index"))
    # Not(Int[nonBool2Int(i), map(nonBool2Int, jk)...] |> sort! |> unique!)
    Not(Int[nonBool2Int(i), map(nonBool2Int, jk)...])
end

#####
##### NamedTuples & Tuples
#####

function Base.getindex(nt::NamedTuple, not::Not{<:Tuple{Vararg{Symbol}}})
    not.skip ⊆ keys(nt) || error(string("type NamedTuple has no fields ", join(not.skip, ", "), ")"))
    nt[filter(∉(not.skip), keys(nt))]
end

function Base.getindex(nt::NamedTuple, not::Not)
    ind = skiprange(not.skip, Base.OneTo(length(nt)), true)
    nt[keys(nt)[ind]]
end

function Base.getindex(tup::Tuple, not::Not)
    ind = skiprange(not.skip, eachindex(tup), true)
    tup[ind]
end

#####
##### SkipRange etc.
#####

struct SkipRange{S,R,T} <: AbstractVector{T}  #  where {R <: AbstractUnitRange{T}}
    skip::S
    range::R
    length::Int  # could delete this field if not supporting out-of-bounds skip
end
Base.length(x::SkipRange) = x.length
Base.size(x::SkipRange) = (length(x),)

# function skiprange(skip::Nothing, range::AbstractRange{T}, check::Bool) where {T}
#     range
# end

function skiprange(skip::Colon, range::AbstractRange{T}, check::Bool) where {T}
    i = first(range)
    i:(i-1)
    # SkipRange{Colon,typeof(range),T}(skip, range, 0)
end
# Base.getindex(x::SkipRange{Colon}, i::Integer) = x.range[i]

function skiprange(skip::Integer, range::AbstractRange{T}, check::Bool) where {T}
    check && checkbounds(range, skip)
    len = length(range) - (skip in eachindex(range))
    SkipRange{typeof(skip),typeof(range),T}(skip, range, len)
end
Base.getindex(x::SkipRange{<:Integer}, i::Integer) = i < x.skip ? x.range[i] : x.range[i+1]

function skiprange(skip::AbstractUnitRange, range::AbstractRange{T}, check::Bool) where {T}
    check && checkbounds(range, skip)
    lo = max(first(skip), firstindex(range))
    hi = min(last(skip), lastindex(range))
    len = length(range) - length(lo:hi)
    SkipRange{UnitRange{Int},typeof(range),T}(lo:hi, range, len)
end
function Base.getindex(x::SkipRange{<:AbstractUnitRange}, i::Integer)
    i < first(x.skip) ? x.range[i] : x.range[i+length(x.skip)]
end

function skiprange(skip::AbstractArray{<:Integer}, range::AbstractRange{T}, check::Bool) where {T}
    if check
        inds = convert(Vector{T}, eachindex(range))
        # deleteat!(inds, unique!(sort(vec(skip))))  # done on construction now
        deleteat!(inds, skip)
    else
        f = in(eachindex(range))
        safe = filter(f, inds)
        skiprange(safe, range, true)
    end
end

function skiprange(skip::AbstractArray{Bool}, range::AbstractRange{T}, check::Bool) where {T}
    @assert check
    # checkbounds(range, skip)
    # findall(!, skip)  # findall(skip) is fast but wrong.
    inds = convert(Vector{T}, eachindex(range))  # does not allow out-of-bounds
    deleteat!(inds, vec(skip))
end

#####
##### Arrays
#####

@inline function Base.to_indices(A, inds, I::Tuple{Not,Vararg{Any}})
    toomany = to_indices(A, inds, (I[1].skip, Base.tail(I)...))  # IDK why they included I[1].skip here
    thisone = skiprange(I[1].skip, inds[1], true)
    (thisone, Base.tail(toomany)...)
end



#####
##### Printing
#####

function Base.show(io::IO, not::Not)
    print(io, "Not(")
    if not.skip isa Tuple
        print(io, ":")
        join(io, not.skip, ", :")
    else
        show(io, not.skip)
    end
    print(io, ")")
end

end # module Exdices
