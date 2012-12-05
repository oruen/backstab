# encoding: utf-8
require 'orm_adapter/ripple'
class User
  include Ripple::Document

  devise :registerable, :database_authenticatable, :validatable

  property :name, String
  property :email, String, :presence => true
  property :password, String
  property :password_confirmation, String
  property :encrypted_password, String

  timestamps!

  def key
    email
  end

  def id
    email
  end

end

