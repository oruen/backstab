class PlanetSystem
  class << self
    def destroy_all
      bucket = Ripple.client["maps"]
      bucket.keys.each {|k| Riak::RObject.new(bucket, k).delete }
    end
  end
end
