
-define(INFO(Msg),error_logger:info_msg(Msg)).
-define(INFO(Format,Msg),error_logger:info_msg(Format,Msg)).

-define(ERROR(Msg),error_logger:error_msg(Msg)).
-define(ERROR(Format,Msg),error_logger:error_msg(Format,Msg)).

-define(DEBUG(Msg),error_logger:info_msg(Msg)).
-define(DEBUG(Format,Msg),error_logger:info_msg(Format,Msg)).
%% -define(DEBUG(Msg),ok).
%% -define(DEBUG_F(Format,Msg),ok).

-define(TRACK(Msg),error_logger:info_msg(Msg)).
-define(TRACK(Format,Msg),error_logger:info_msg(Format,Msg)).