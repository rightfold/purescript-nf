'use strict';

function deepseqFFI(nf) {
    return function(a) {
        return function(b) {
            nf(a);
            return b;
        };
    };
};

exports.deepseqFFI = deepseqFFI;
