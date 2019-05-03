require "spec"
require "./the_feast_of_many_beasts"

describe "#feast" do
  it "correctly decides if the dish name matches the beast name" do
    feast("great blue heron", "garlic naan").should be_true
    feast("chickadee", "chocolate cake").should be_true
    feast("brown bear", "bear claw").should be_false
    feast("marmot", "mulberry tart").should be_true
    feast("porcupine", "pie").should be_true
    feast("electric eel", "lasagna").should be_false
    feast("slow loris", "salsas").should be_true
    feast("ox", "orange lox").should be_true
    feast("blue-footed booby", "blueberry").should be_true
    feast("blue-footed booby", "binary human").should be_false
  end
end
