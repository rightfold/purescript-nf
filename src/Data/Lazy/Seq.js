'use strict';

function seqFFI(force) {
    return function(a) {
        return function(b) {
            force(a);
            return b;
        };
    };
};

exports.seqFFI = seqFFi;
