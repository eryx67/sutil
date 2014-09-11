%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2013, Vladimir G. Sekissov
%%% @doc
%%% Общие определения
%%% @end
%%% Created : 22 Nov 2013 by Vladimir G. Sekissov <eryx67@gmail.com>

-define(maybe_lift(Expr), sutil:maybe_lift(fun() -> Expr end)).
-define(maybe_unlift(Expr), sutil:maybe_unlift(fun() -> Expr end)).
