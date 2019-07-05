#! /usr/bin/env ruby
require 'logger'
require 'fileutils'
require 'zip'

SYNTH_DIR = File.expand_path("../Synthesizer/", File.dirname(__FILE__))
PROFILE_DIR = File.expand_path("profiles", SYNTH_DIR)
$LOGGER = Logger.new(STDERR)

# delete profile data before running the KaniCUDA so that we can only
# copy the newly created profile data
def delete_profile_dir()
  $LOGGER.debug "delete_profile_dir: #{PROFILE_DIR}"
  if Dir.exist? PROFILE_DIR
    $LOGGER.debug "#{PROFILE_DIR} exists"
    FileUtils.rm_r(PROFILE_DIR)
    Dir.exist? PROFILE_DIR and (raise "failed to delete #{PROFILE_DIR}")
    $LOGGER.debug "#{PROFILE_DIR} successfully deleted"
  end
end

# copy the profile data into the test directory
def save_profiles()
  $LOGGER.debug "saving profile data"
  save_path = "./saved/"
  Dir.exist? PROFILE_DIR or (raise "no profile data found in #{PROFILE_DIR}")
  FileUtils.cp_r(PROFILE_DIR, save_path+"/profiles", :preserve=>true)
  FileUtils.cp_r(SYNTH_DIR+"/out.rkt", save_path, :preserve=>true)
end

# run KaniCUDA
def run()
  Dir.chdir SYNTH_DIR do
    system("sh ./synth.sh ../benchmarks/diffusion3d_A.cu")
  end
end

# run KaniCUDA and copy the profile data
def record_profiles()
  delete_profile_dir
  run()
  save_profiles()
end

class Out_Rkt
  def initialize(path) @path=path end
  def contents() @contents || (@contents = open(@path).read) end
  def dirname() File.dirname(@path) end
  
  PROFILE_PATTERN = /__opt__\d+/
  def profile_basenames
    ["vars"]+self.contents.scan(PROFILE_PATTERN)
  end

  def profile_files
    self.profile_basenames.map{ | fname |
      File.expand_path(fname, self.dirname+"/profiles")
    }
  end
  def contents_sans_random
    self.contents.gsub(PROFILE_PATTERN,"")
  end

  def compare_strings(this,that, title)
    $LOGGER.debug "Comparing #{title}"
    r = this == that
    $LOGGER.debug "result #{r}"
    r
  end
  def compare_body(other)
    compare_strings(self.contents_sans_random, other.contents_sans_random,
                    "body of #{self}")
  end
  def compare_profiles(other)
    self.profile_files.zip(other.profile_files).all? do | this_p, other_p |
      compare_strings(open(this_p).read, open(other_p).read,
                      "profiles #{File.basename(this_p)} #{File.basename(other_p)}")
    end
  end
  def compare(other)
    compare_body(other) && compare_profiles(other)
  end
  def to_s() @path end
end

# compare the previously recorded profile data against the newly
# created one
def compare_profiles()
  save_path = "./saved" # previously recorded
  saved_prof = Out_Rkt.new(File.expand_path("out.rkt", save_path))
  new_prof = Out_Rkt.new(File.expand_path("out.rkt", SYNTH_DIR))
  saved_prof.compare(new_prof) or
    ($LOGGER.error "profiles #{saved_prof} and #{new_prof} differ")
end

def test_profiles()
  delete_profile_dir
  run()
  compare_profiles()
end

test_profiles
#record_profiles
