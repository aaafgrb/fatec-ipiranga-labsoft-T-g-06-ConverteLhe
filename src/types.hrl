
-include("throws.hrl").

-define(STR_TO_INTEGER, 
  fun(V) -> 
    {R, Rest} = string:to_integer(V),
    case Rest of
      "" -> R;
      _ -> throw(?EX_TYPE_PARSE_ERROR)
    end
  end).

-define(STR_TO_FLOAT,   
  fun(V) -> 
    {R, Rest} = string:to_float(V),
    case Rest of
      "" -> R;
      _ -> throw(?EX_TYPE_PARSE_ERROR)
    end
  end).

-define(IDENTITY, fun(V) -> V end).

-define(TYPE_LIST, [
  {105, ?STR_TO_INTEGER}, %'i'
  {102, ?STR_TO_FLOAT},   %'f'
  {115, ?IDENTITY}        %'s'
  ]).