# encoding: utf-8
require 'devise/orm/ripple'
class User
  include Ripple::Document

  before_create do |record|
    record.color = "%06x" % (rand * 0xffffff)
    record.token = SecureRandom.hex
  end

  after_create do |record|
    conn = Faraday.new(:url => "http://127.0.0.1:8080/create_planet?token=#{record.token}") do |faraday|
      faraday.request  :url_encoded
      faraday.response :logger, Rails.logger
      faraday.adapter  Faraday.default_adapter
    end
    conn.post
  end

  devise :registerable, :database_authenticatable, :validatable

  property :name, String, :presence => true
  property :email, String, :presence => true, :index => true
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
    token
  end

  def id
    token
  end
end

