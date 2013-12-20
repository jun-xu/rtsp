-record(url, {protocol :: string(),
			  host :: string(),
			  port :: integer(),
			  path = [] :: list(string()),
			  params = [] :: list(tuple())}).