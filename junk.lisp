key <- (kspace / kreturn / km / kf8 / kf9 / kf10 / kf11 / kf12 / kright / kleft
              / kup / kdown / kwheel-up / kwheel-down)


#|
key <- (kspace / kreturn / km / kf8 / kf9 / kf10 / kf11 / kf12 / kright / kleft
              / kup / kdown / kwheel-up / kwheel-down);
|#


pkeying <- clause:key-clause+ -> clause;
key-clause <- k:key _* ':' _* e:sexp -> (list k e);
