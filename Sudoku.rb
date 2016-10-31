GRD   = 0
ROW   = 1
COL   = 2
BOX   = 3
VIEW  = 4
UNIT  = 3
SIZE  = UNIT * UNIT
COORD = SIZE * SIZE * SIZE
BLOCK = VIEW * SIZE * SIZE

OPEN   = 0
FIXED  = 1
BANNED = 2

DONE = SIZE + 1

DIGITS  = "123456789"

def coord(i, j, k)
  return (i * SIZE + j) * SIZE + k
end

def block(v, p, q)
  return (v * SIZE + p) * SIZE + q
end

PARENTS = Array.new(COORD)
SIZE.times do |i|
  SIZE.times do |j|
    p = i / UNIT * UNIT + j / UNIT
    SIZE.times do |k|
      PARENTS[coord(i, j, k)] = [
        block(GRD, i, j),
        block(ROW, i, k),
        block(COL, j, k),
        block(BOX, p, k),
      ]
    end
  end
end

CHILDREN = Array.new(BLOCK)
BLOCK.times do |b|
  CHILDREN[b] = []
end
COORD.times do |c|
  PARENTS[c].each do |b|
    CHILDREN[b].push(c)
  end
end

class NoSolutionError < StandardError
end

class Sudoku

  attr_accessor :state, :count, :queue

  def initialize(that = nil)
    if that == nil
      @state = Array.new(COORD, OPEN)
      @count = Array.new(BLOCK, SIZE)
    else
      @state = that.state.dup
      @count = that.count.dup
    end
    @queue = Array.new
  end

  def fix(c)
    @queue.push(c)
    return self
  end

  def read(input)
    SIZE.times do |i|
      SIZE.times do |j|
        if i * SIZE + j < input.length
          k = DIGITS.index(input[i * SIZE + j])
          fix(coord(i, j, k)) unless k.nil?
        end
      end
    end
    return self
  end

  def search

    until @queue.empty?
      c0 = @queue.shift
      next if @state[c0] == FIXED
      raise NoSolutionError.new if @state[c0] == BANNED
      @state[c0] = FIXED
      PARENTS[c0].each do |b1|
        @count[b1] = DONE
        CHILDREN[b1].each do |c2|
          if c2 != c0 and @state[c2] == OPEN
            @state[c2] = BANNED
            PARENTS[c2].each do |b3|
              if b3 != b1
                @count[b3] -= 1
                raise NoSolutionError.new if @count[b3] == 0
                if @count[b3] == 1
                  CHILDREN[b3].each do |c4|
                    fix(c4) if @state[c4] == OPEN
                  end
                end
              end
            end
          end
        end
      end
    end

    m = @count.min
    return self if m == DONE

    b = @count.index(m)
    CHILDREN[b].each do |c|
      if @state[c] == OPEN
        begin
          return Sudoku.new(self).fix(c).search
        rescue NoSolutionError
        end
      end
    end

    raise NoSolutionError.new
  end

  def to_s
    output = Array.new(SIZE * SIZE, ".")
    SIZE.times do |i|
      SIZE.times do |j|
        SIZE.times do |k|
          if @state[coord(i, j, k)] == FIXED
            output[i * SIZE + j] = DIGITS[k]
          end
        end
      end
    end
    output.join
  end
end

def solve(input)
  begin
    return Sudoku.new.read(input).search.to_s
  rescue NoSolutionError
    return "NO SOLUTION"
  end
end

loop do
  input = $stdin.gets
  break if input.nil?
  puts solve(input.chomp)
end
