# encoding: utf-8
require 'devise/orm/ripple'
class User
  include Ripple::Document

  before_create :set_color

  devise :registerable, :database_authenticatable, :validatable

  property :name, String
  property :email, String, :presence => true
  property :password, String
  property :password_confirmation, String
  property :encrypted_password, String
  property :color, String

  timestamps!

  def key
    email
  end

  def id
    email
  end

  private
  def set_color
    self.color = "%06x" % (rand * 0xffffff)
  end
end

