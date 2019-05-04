require "spec"
require "./decompose_dna_into_reading_frames"

describe DnaStrand do
  describe "#frame" do
    it "returns the requested frame" do
      dna_strand = DnaStrand.new("AGGTGACACCGCAAGCCTTATATTAGC")

      dna_strand.frame(1_u8).should eq "AGG TGA CAC CGC AAG CCT TAT ATT AGC"
      dna_strand.frame(2_u8).should eq "A GGT GAC ACC GCA AGC CTT ATA TTA GC"
      dna_strand.frame(3_u8).should eq "AG GTG ACA CCG CAA GCC TTA TAT TAG C"
    end
  end

  describe "#frames" do
    it "returns all the three frames as a hash" do
      dna_strand = DnaStrand.new("AGGTGACACCGCAAGCCTTATATTAGC")

      dna_strand.frames.should eq({
        1 => "AGG TGA CAC CGC AAG CCT TAT ATT AGC",
        2 => "A GGT GAC ACC GCA AGC CTT ATA TTA GC",
        3 => "AG GTG ACA CCG CAA GCC TTA TAT TAG C",
      })
    end
  end
end

describe "#decompose_single_strand" do
  it "correctly decomposes the DNA strand into 3 reading frames" do
    decompose_single_strand("AGGTGACACCGCAAGCCTTATATTAGC").should eq <<-STRING
      Frame 1: AGG TGA CAC CGC AAG CCT TAT ATT AGC
      Frame 2: A GGT GAC ACC GCA AGC CTT ATA TTA GC
      Frame 3: AG GTG ACA CCG CAA GCC TTA TAT TAG C
      STRING
  end
end
