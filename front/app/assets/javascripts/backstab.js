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
goog.require('d3');

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

backstab.renderGlobalMap = function(data) {
  window.D = data;
  var width = 960,
      height = 500;

  var nodes = data.map(function(d) {return {radius: d[1][1].length}});

  var force = d3.layout.force()
      .gravity(0.05)
      .charge(-100)
      .nodes(nodes)
      .size([width, height]);

  force.start();

  var canvas = d3.select("body").append("canvas")
      .attr("width", width)
      .attr("height", height);

  var context = canvas.node().getContext("2d");

  force.on("tick", function(e) {
    var q = d3.geom.quadtree(nodes),
        i,
        d,
        n = nodes.length;

    for (i = 1; i < n; ++i) q.visit(collide(nodes[i]));

    context.clearRect(0, 0, width, height);
    context.fillStyle = "steelblue";
    context.beginPath();
    for (i = 1; i < n; ++i) {
      d = nodes[i];
      context.moveTo(d.x, d.y);
      context.arc(d.x, d.y, d.radius, 0, 2 * Math.PI);
    }
    context.fill();
  });

  function collide(node) {
    var r = node.radius + 16,
        nx1 = node.x - r,
        nx2 = node.x + r,
        ny1 = node.y - r,
        ny2 = node.y + r;
    return function(quad, x1, y1, x2, y2) {
      if (quad.point && (quad.point !== node)) {
        var x = node.x - quad.point.x,
            y = node.y - quad.point.y,
            l = Math.sqrt(x * x + y * y),
            r = node.radius + quad.point.radius;
        if (l < r) {
          l = (l - r) / l * .5;
          node.x -= x *= l;
          node.y -= y *= l;
          quad.point.x += x;
          quad.point.y += y;
        }
      }
      return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
    };
  }

};


//this is required for outside access after code is compiled in ADVANCED_COMPILATIONS mode
goog.exportSymbol('backstab.start', backstab.start);

window.onload = backstab.start;
