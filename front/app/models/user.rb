# encoding: utf-8
require 'devise/orm/ripple'
class User
  include Ripple::Document

  before_create do |record|
    record.color = "%06x" % (rand * 0xffffff)
    record.token = SecureRandom.hex
  end

  devise :registerable, :database_authenticatable, :validatable

  property :name, String
  property :email, String, :presence => true
  property :password, String
  property :password_confirmation, String
  property :encrypted_password, String
  property :color, String
  property :token, String

  timestamps!

  class << self
    def destroy_all
      bucket.keys.each {|k| Riak::RObject.new(bucket, k).delete }
    end
  end

  def key
    email
  end

  def id
    email
  end
end

