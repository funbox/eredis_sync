local aTempKey = "a-temp-key"
local cycles
redis.call("SET",aTempKey,"1")
redis.call("PEXPIRE",aTempKey, 100)
for i = 0, ARGV[1], 1 do
	local apttl = redis.call("PTTL",aTempKey)
	cycles = i;
	if apttl == 0 then
		break;
	end
end
return cycles
