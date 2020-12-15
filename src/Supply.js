`use strict`;

exports.unsafeInterleaveEffect = function(f) {
  return function() { return {
    get val() {
      if (this.cachedval === undefined) {
        this.cachedval = f();
      }
      return this.cachedval;
    }
  };};
};
