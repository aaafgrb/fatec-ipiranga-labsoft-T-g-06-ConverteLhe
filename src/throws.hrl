-define(EX_TYPE_PARSE_ERROR, 1001).
-define(EX_UNSUPPORTED_TYPE, 1002).
-define(EX_UNSUPPORTED_ARROW, 1003).
-define(EX_ARROW_ERROR, 1004).
-define(EX_EMPTY_ARGUMENT, 1005).




-define(EXCEPTION_LIST, [
  {?EX_TYPE_PARSE_ERROR, "parse fail"},
  {?EX_UNSUPPORTED_TYPE, "couldn't find type"},
  {?EX_UNSUPPORTED_ARROW, "couldn't find arrow"},
  {?EX_ARROW_ERROR, "arrow error"},
  {?EX_EMPTY_ARGUMENT, "expected argument but recieved nothing"},
  ]).
