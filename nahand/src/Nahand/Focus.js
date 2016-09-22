exports.setFocusImpl = function(elemId) {
  return function() {
    var e = document.getElementById(elemId);
    if (e) {
      e.focus();
    }
  };
};
