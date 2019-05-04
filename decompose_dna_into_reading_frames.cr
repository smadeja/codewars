class DnaStrand
  def initialize(@representation : String)
  end

  def frame(frame_number : UInt8) : String
    String.build do |frame|
      @representation.each_char_with_index do |char, index|
        frame << char

        if (index < @representation.size - 1) & (index % 3 == (frame_number + 1) % 3)
          frame << ' '
        end
      end
    end
  end

  def frames
    frames = Hash(UInt8, String).new

    1_u8.upto(3) do |i|
      frames[i] = frame(i)
    end

    frames
  end
end

def decompose_single_strand(dna_strand_representation)
  DnaStrand.new(dna_strand_representation).frames.map do |key, value|
    "Frame #{key}: #{value}"
  end.join("\n")
end
