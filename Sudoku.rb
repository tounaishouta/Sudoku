UNIT    = 3
SIZE    = UNIT * UNIT
GRD     = 0
ROW     = 1
COL     = 2
BOX     = 3
VIEW    = 4
COORD   = SIZE * SIZE * SIZE
BLOCK   = VIEW * SIZE * SIZE
DEFINED = 0xDEF
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

  attr_accessor :admit, :count

  def initialize(that = nil)
    if that == nil
      @admit = Array.new(COORD, true)
      @count = Array.new(BLOCK, SIZE)
    else
      @admit = that.admit.dup
      @count = that.count.dup
    end
  end

  def read(input)
    [SIZE * SIZE, input.length].min.times do |ij|
      k = DIGITS.index(input[ij])
      assign(coord(ij / SIZE, ij % SIZE, k)) if k != nil
    end
    return self
  end

  def search

    m = @count.min
    return self if m == DEFINED

    b = @count.index(m)
    CHILDREN[b].each do |c|
      if @admit[c]
        begin
          return Sudoku.new(self).assign(c).search
        rescue NoSolutionError
        end
      end
    end

    raise NoSolutionError.new

  end

  def assign(c)
    queue = [c]
    while not queue.empty?
      c0 = queue.shift
      raise NoSolutionError.new if not @admit[c0]
      PARENTS[c0].each do |b1|
        @count[b1] = DEFINED
        CHILDREN[b1].each do |c2|
          if c2 != c0 and @admit[c2]
            @admit[c2] = false
            PARENTS[c2].each do |b3|
              if b3 != b1
                @count[b3] -= 1
                if @count[b3] == 0
                  raise NoSolutionError.new
                elsif @count[b3] == 1
                  CHILDREN[b3].each do |c4|
                    queue.push[c4] if @admit[c4]
                  end
                end
              end
            end
          end
        end
      end
    end
    return self
  end

  def to_s
    output = ""
    SIZE.times do |i|
      SIZE.times do |j|
        ks = []
        SIZE.times do |k|
          ks.push(k) if @admit[coord(i, j, k)]
        end
        if ks.length == 1
          output << DIGITS[ks.first]
        else
          output << "."
        end
      end
    end
    output
  end

end

def solve(input)
  begin
    return Sudoku.new.read(input).search
  rescue NoSolutionError
    return "NO SOLUTION"
  end
end

loop do
  input = $stdin.gets
  break if input == nil
  puts solve(input.chomp)
end
