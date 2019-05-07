class OddArray
  def initialize(@array : Array(Int32)); end

  def find_odd_ones : Array(Int32)
    counts = Hash(Int32, Int32).new

    @array.each do |value|
      counts[value] = counts.fetch(value, 0) + 1
    end

    counts.select { |key, value| value.odd? }.keys
  end
end

def find_it(array : Array(Int32)) : Int32 | NoReturn
  OddArray.new(array).find_odd_ones.first
end
