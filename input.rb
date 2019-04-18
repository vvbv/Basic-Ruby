
def enIntervalo(min,max,value)

	if (min < value) then
		if ( value < max  ) then
			return true;
		else
			return false;
		end
	else
		return false;
	end
		

end

declare min, max, value, w;
min = 50;
max = 100;
value = 75;
w = enIntervalo(min, max, value);
puts w;

if  w  then
	puts "En el intervalo";
end
	
until w do
	puts "Fuera del intervalo";
end