Coord = Int
Block = Int
State = Int
Count = Int
struct NoSolutionException <: Exception end
struct Sudoku
    state::Array{State, 1}
    count::Array{Count, 1}
    queue::Array{Coord, 1}
end

M = 3
N = M * M
N_COORD = N * N * N
N_BLOCK = 4 * N * N

OPEN   = 0
FIXED  = 1
BANNED = 2

CLOSED = N + 1

DIGITS = "123456789"

coord(i, j, k) = ((i - 1) * N + j - 1) * N + k
block(v, p, q) = ((v - 1) * N + p - 1) * N + q

parents = zeros(Block, N_COORD, 4)
for i in 1:N, j in 1:N, k in 1:N
    c = coord(i, j, k)
    p = div(i - 1, M) * M + div(j - 1, M) + 1
    parents[c, 1] = block(1, i, j)
    parents[c, 2] = block(2, i, k)
    parents[c, 3] = block(3, j, k)
    parents[c, 4] = block(4, p, k)
end

children = zeros(Coord, N_BLOCK, N)
for b in 1:N_BLOCK
    children[b, :] = find(any(parents .== b, 2))
end

Sudoku() = begin
    state = fill(OPEN, N_COORD)
    count = fill(N, N_BLOCK)
    queue = Coord[]
    Sudoku(state, count, queue)
end

Sudoku(str::String) = begin
    s = Sudoku()
    for i in 1:N, j in 1:N
        ij = (i - 1) * N + j
        if ij <= length(str)
            k = search(DIGITS, str[ij])
            if k != 0
                push!(s.queue, coord(i, j, k))
            end
        end
    end
    s
end

Sudoku(s::Sudoku) = begin
    Sudoku(copy(s.state), copy(s.count), copy(s.queue))
end

show(s::Sudoku) = begin
    arr = fill('.', N, N)
    for i in 1:N, j in 1:N, k in 1:N
        if s.state[coord(i, j, k)] == FIXED
            arr[j, i] = DIGITS[k]
        end
    end
    join(arr)
end

solve(str::String) = begin
    try
        show(search!(Sudoku(str)))
    catch ME
        if ME isa NoSolutionException
            "NO SOLUTION"
        else
            rethrow(ME)
        end
    end
end

search!(s::Sudoku) = begin

    while !isempty(s.queue)
        c = shift!(s.queue)
        if s.state[c] == OPEN
            fix!(s, c)
        elseif s.state[c] == FIXED
            continue
        elseif s.state[c] == BANNED
            throw(NoSolutionException())
        end
    end

    m, b = findmin(s.count)
    if m == CLOSED
        return s
    end

    for c in children[b, :]
        if s.state[c] == OPEN
            try
                ss = Sudoku(s)
                push!(ss.queue, c)
                return search!(ss)
            catch ME
                if !(ME isa NoSolutionException)
                    rethrow(ME)
                end
            end
        end
    end

    throw(NoSolutionException())
end

fix!(s::Sudoku, c::Coord) = begin
    s.state[c] = FIXED
    for b in parents[c, :]
        close!(s, b)
    end
end

close!(s::Sudoku, b::Block) = begin
    s.count[b] = CLOSED
    for c in children[b, :]
        if s.state[c] == OPEN
            ban!(s, c)
        end
    end
end

ban!(s::Sudoku, c::Coord) = begin
    s.state[c] = BANNED
    for b in parents[c, :]
        if s.count[b] != CLOSED
            countdown!(s, b)
        end
    end
end

countdown!(s::Sudoku, b::Block) = begin
    s.count[b] -= 1
    if s.count[b] == 0
        throw(NoSolutionException())
    elseif s.count[b] == 1
        for c in children[b, :]
            if s.state[c] == OPEN
                push!(s.queue, c)
            end
        end
    end
end

for str in eachline()
    println(solve(str))
end
