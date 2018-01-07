M = 3
N = M * M

N_COORD = N * N * N
N_BLOCK = 4 * N * N

coord(i, j, k) = ((i - 1) * N + j - 1) * N + k

block(i, j, k) = begin
    p = div(i - 1, M) * M + div(j - 1, M) + 1
    [ (0 * N + i - 1) * N + j
    , (1 * N + i - 1) * N + k
    , (2 * N + j - 1) * N + k
    , (3 * N + p - 1) * N + k
    ]
end

parents = zeros(Int, N_COORD, 4)
for i in 1:N, j in 1:N, k in 1:N
    parents[coord(i, j, k), :] = block(i, j, k)
end

children = zeros(Int, N_BLOCK, N)
for b in 1:N_BLOCK
    children[b, :] = find(any(parents .== b, 2))
end

DIGITS = "123456789"

OPEN   = 0
FIXED  = 1
BANNED = 2

CLOSED = N + 1

Coord = Int
Block = Int
State = Int
Count = Int

struct Sudoku
    state::Array{State, 1}
    count::Array{Count, 1}
    queue::Array{Coord, 1}
end
Sudoku() = begin
    state = fill(OPEN, N_COORD)
    count = fill(N, N_BLOCK)
    queue = Coord[]
    Sudoku(state, count, queue)
end
Sudoku(s::Sudoku) = begin
    Sudoku(copy(s.state), copy(s.count), copy(s.queue))
end
Sudoku(str::String) = begin
    s = Sudoku()
    for i in 1:N, j in 1:N
        ij = (i - 1) * N + j
        if ij <= length(str)
            k = search(DIGITS, str[ij])
            if k != 0
                enqueue!(s, coord(i, j, k))
            end
        end
    end
    s
end

struct NoSolutionException <: Exception end

show(s::Sudoku) = begin
    res = fill('.', N, N)
    for i in 1:N, j in 1:N, k in 1:N
        if s.state[coord(i, j, k)] == FIXED
            res[j, i] = DIGITS[k]
        end
    end
    join(res)
end

enqueue!(s::Sudoku, c::Coord) = begin
    push!(s.queue, c)
    s
end

solve!(s::Sudoku) = begin

    while !isempty(s.queue)
        c = shift!(s.queue)
        fix!(s, c)
    end

    m, b = findmin(s.count)
    if m == CLOSED
        return s
    end

    for c in children[b, :]
        if s.state[c] == OPEN
            try
                return solve!(enqueue!(Sudoku(s), c))
            catch ME
                if !(ME isa NoSolutionException)
                    throw(ME)
                end
            end
        end
    end

    throw(NoSolutionException())
end

fix!(s::Sudoku, c::Coord) = begin
    if s.state[c] == FIXED
        return
    elseif s.state[c] == BANNED
        throw(NoSolutionException())
    end
    s.state[c] = FIXED
    for b in parents[c, :]
        mark!(s, b)
    end
end

mark!(s::Sudoku, b::Block) = begin
    if s.count[b] == CLOSED
        throw(NoSolutionException())
    end
    s.count[b] = CLOSED
    for c in children[b, :]
        ban!(s, c)
    end
end

ban!(s::Sudoku, c::Coord) = begin
    if s.state[c] != OPEN
        return
    end
    s.state[c] = BANNED
    for b in parents[c, :]
        countdown!(s, b)
    end
end

countdown!(s::Sudoku, b::Block) = begin
    if s.count[b] == CLOSED
        return
    end
    s.count[b] -= 1
    if s.count[b] == 0
        throw(NoSolutionException())
    elseif s.count[b] == 1
        for c in children[b, :]
            if s.state[c] == OPEN
                enqueue!(s, c)
            end
        end
    end
end

for str in eachline()
    try
        println(show(solve!(Sudoku(str))))
    catch ME
        if ME isa NoSolutionException
            println("NO SOLUTION")
        else
            throw(ME)
        end
    end
end
