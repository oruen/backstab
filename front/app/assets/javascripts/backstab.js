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
goog.require('backstab.PlanetSystem');
goog.require('backstab.User');
goog.require('backstab.EventHandler');
goog.require('d3');

// entrypoint
backstab.start = function(){
  backstab.currentMap = null;

	backstab.director = new lime.Director(document.body,1024,768);
	var scene = new lime.Scene();
  //scene.setRenderer(lime.Renderer.CANVAS);

	backstab.director.makeMobileWebAppCapable();

	// set current scene active
	backstab.director.replaceScene(scene);

  backstab.wsHandler = new backstab.WsHandler(Userinfo.token);
  new backstab.EventHandler(backstab.wsHandler, scene);
  backstab.wsHandler.connect();
};

backstab.send = function(msg) {
  this.wsHandler.send(msg);
};

backstab.hideBattleScene = function() {
  this.director.domElement.style["display"] = "none";
};

backstab.showBattleScene = function() {
  this.director.domElement.style["display"] = "block";
};

backstab.renderGlobalMap = function(planetSystems) {
  this.hideBattleScene();
  var width = 960,
      height = 500;

  var nodes = planetSystems.map(function(d) {return {radius: d.planets.length * 2 + 10, userId: d.userId}}),
      color = d3.scale.category10();

  var force = d3.layout.force()
      .gravity(0.02)
      .charge(-100)
      .nodes(nodes)
      .size([width, height]);

  force.start();

  var svg = d3.select("body").append("svg")
      .attr("width", width)
      .attr("height", height);

  svg.selectAll("circle")
      .data(nodes)
    .enter().append("svg:circle")
      .attr("r", function(d) { return d.radius; })
      .style("fill", function(d, i) { return color(i % 3); })
      .style("stroke", function(d, i) {console.log(d);return d.userId === Userinfo.token ? "#000" : color(i % 3);})
      .on("click", function() {console.log("clicked", arguments)})
      .call(force.drag);

  force.on("tick", function(e) {
    var q = d3.geom.quadtree(nodes),
        i = 0,
        n = nodes.length;

    while (++i < n) {
      q.visit(collide(nodes[i]));
    }

    svg.selectAll("circle")
        .attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
  });

  function collide(node) {
    var r = node.radius + 40,
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
