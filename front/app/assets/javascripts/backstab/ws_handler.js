goog.provide('backstab.WsHandler');
goog.provide('backstab.WsHandler.EventType');

goog.require('goog.net.WebSocket');
goog.require('goog.events');
goog.require('goog.events.EventTarget');

backstab.WsHandler = function() {
  goog.events.EventTarget.call(this);
  this.reader = new FileReader();
  this.reader.onload = goog.bind(function(event) {
    var decodedMsg = Bert.decode(event.target.result);
    console.log("Msg unpacked", decodedMsg);
    var event = new goog.events.Event(backstab.WsHandler.EventType[decodedMsg[0].value.toUpperCase()], decodedMsg[1]);
    this.dispatchEvent(event);
  }, this);
  this.reader.onerror = function() {
    console.log("File read error", arguments);
  };
}

goog.inherits(backstab.WsHandler, goog.events.EventTarget);

backstab.WsHandler.EventType = {
  MAP: goog.events.getUniqueId('map'),
  USERINFO: goog.events.getUniqueId('userinfo'),
  GOTO: goog.events.getUniqueId('goto')
};

backstab.WsHandler.prototype.onOpen = function() {
  console.log("WS opened", arguments);
};

backstab.WsHandler.prototype.onMessage = function(msg) {
  console.log("Message came", msg);
  this.reader.readAsBinaryString(msg.message);
};

backstab.WsHandler.prototype.onClose = function() {
  console.log("WS closed", arguments);
};

backstab.WsHandler.prototype.connect = function() {
  this._ws = new goog.net.WebSocket(false);
  goog.events.listen(this._ws, goog.net.WebSocket.EventType.OPENED, this.onOpen);
  goog.events.listen(this._ws, goog.net.WebSocket.EventType.MESSAGE, goog.bind(this.onMessage, this));
  goog.events.listen(this._ws, goog.net.WebSocket.EventType.CLOSED, this.onClose);
  this._ws.open("ws://localhost:8080/ws");
};

backstab.WsHandler.prototype.send = function(msg) {
  console.log("Sending", msg);
  this._ws.send(new Blob([Bert.encode(msg)]));
};