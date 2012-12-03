require 'ripple'

module OrmAdapter
  class Ripple < Base
    # Do not consider these to be part of the class list
    def self.except_classes
      @@except_classes ||= []
    end

    # Gets a list of the available models for this adapter
    def self.model_classes
      ObjectSpace.each_object(Class).to_a.select {|klass| klass.ancestors.include? Ripple::Document}
    end

    # get a list of column names for a given class
    def column_names
      klass.properties.keys
    end

    # Get an instance by id of the model
    def get!(id)
      klass.find(wrap_key(id))
    end

    # Get an instance by id of the model
    def get(id)
      klass.find(wrap_key(id))
    end

    # Find the first instance matching conditions
    def find_first(conditions)
      klass.find(keys_for_conditions(conditions).first)
    end

    # Find all models matching conditions
    def find_all(conditions)
      klass.find(keys_for_conditions(conditions)) or []
    end

    # Create a model with given attributes
    def create!(attributes)
      klass.create!(attributes)
    end

  protected
    
    def keys_for_conditions(conditions)
      map = "
        function(v) {
          if (v.values) {
            original = v;
            var v = Riak.mapValuesJson(v)[0];
            return (#{conditions.map { |k,v| "v.#{k} === '#{v}'" }.join(' && ')}) ? [decodeURIComponent(original.key)] : [];
          } else return [];
        }
      "
      keys = Riak::MapReduce.new(klass.bucket.client).add(klass.bucket).map(map, :keep => true).run
    end
    
  end
end

ActiveSupport.on_load(:active_record) do
  extend ::OrmAdapter::ToAdapter
  self::OrmAdapter = ::OrmAdapter::Ripple
end

