UNIT  = 3
VIEW  = 4
SIZE  = UNIT * UNIT
COORD = SIZE * SIZE * SIZE
BLOCK = VIEW * SIZE * SIZE

module View
  GRD = 0
  ROW = 1
  COL = 2
  BOX = 3
end

module State
  OPEN   = 0
  FIXED  = 1
  BANNED = 2
end

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
        block(View::GRD, i, j),
        block(View::ROW, i, k),
        block(View::COL, j, k),
        block(View::BOX, p, k),
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
      @state = Array.new(COORD, State::OPEN)
      @count = Array.new(BLOCK, SIZE)
    else
      @state = that.state.dup
      @count = that.count.dup
    end
    @queue = Array.new
  end

  def enqueue(c)
    @queue.push(c)
    return self
  end

  def read(input)
    SIZE.times do |i|
      SIZE.times do |j|
        if i * SIZE + j < input.length
          k = DIGITS.index(input[i * SIZE + j])
          enqueue(coord(i, j, k)) unless k.nil?
        end
      end
    end
    return self
  end

  def to_s
    output = Array.new(SIZE * SIZE, ".")
    SIZE.times do |i|
      SIZE.times do |j|
        SIZE.times do |k|
          if @state[coord(i, j, k)] == State::FIXED
            output[i * SIZE + j] = DIGITS[k]
          end
        end
      end
    end
    output.join
  end

  def search

    until @queue.empty?
      c = @queue.shift
      case @state[c]
      when State::OPEN
        fix(c)
      when State::FIXED
        nil
      when State::BANNED
        raise NoSolutionError.new
      end
    end

    m = @count.min
    return self if m == DONE

    b = @count.index(m)
    CHILDREN[b].each do |c|
      if @state[c] == State::OPEN
        begin
          return Sudoku.new(self).enqueue(c).search
        rescue NoSolutionError
        end
      end
    end

    raise NoSolutionError.new
  end

  def fix(c)
    @state[c] = State::FIXED
    PARENTS[c].each { |b| mark(b) }
  end

  def mark(b)
    @count[b] = DONE
    CHILDREN[b].each { |c| ban(c) if @state[c] == State::OPEN }
  end

  def ban(c)
    @state[c] = State::BANNED
    PARENTS[c].each { |b| countdown(b) if @count[b] != DONE }
  end

  def countdown(b)
    @count[b] -= 1
    case @count[b]
    when 0
      raise NoSolutionError.new
    when 1
      CHILDREN[b].each { |c| enqueue(c) if @state[c] == State::OPEN }
    end
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
