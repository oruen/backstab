//set main namespace
goog.provide('backstab');


//get requirements
goog.require('lime.Director');
goog.require('lime.Scene');
goog.require('lime.Layer');
goog.require('lime.Circle');
goog.require('lime.Label');
goog.require('lime.animation.Spawn');
goog.require('lime.animation.FadeTo');
goog.require('lime.animation.ScaleTo');
goog.require('lime.animation.MoveTo');
goog.require('backstab.WsHandler');
goog.require('backstab.Bert');
goog.require('backstab.Planet');
goog.require('backstab.BattleMap');
goog.require('backstab.User');
goog.require('backstab.EventHandler');

// entrypoint
backstab.start = function(){
  backstab.currentMap = null;

	var director = new lime.Director(document.body,1024,768),
	    scene = new lime.Scene();
  //scene.setRenderer(lime.Renderer.CANVAS);

	director.makeMobileWebAppCapable();

	// set current scene active
	director.replaceScene(scene);

  backstab.wsHandler = new backstab.WsHandler();
  new backstab.EventHandler(backstab.wsHandler, scene);
  backstab.wsHandler.connect();
};

backstab.send = function(msg) {
  this.wsHandler.send(msg);
};


//this is required for outside access after code is compiled in ADVANCED_COMPILATIONS mode
goog.exportSymbol('backstab.start', backstab.start);

window.onload = backstab.start;
