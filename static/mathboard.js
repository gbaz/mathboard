$(document).ready(function() {
mathboard = (function() {
    var mbd = $("<div id='drawing'></div>");
    $("body").append(mbd);
    var mb = SVG('drawing').size(300, 300);
    return {
	addSVG : function(t) {
	    var newElement = SVG.adopt($(t)[0]);
	    var g = mb.group();
	    newElement.toParent(g);
	}
    };
})();
});
