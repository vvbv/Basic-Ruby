
def enIntervalo(min,max,value)
	declare to_return;
	to_return = false;
	if (min < value) then
		if (value < max) then
			to_return = true;
		end
	end
	return to_return;
end

declare min, max, value, w;

w = enIntervalo(1, 10, 5);
if w then
	puts "En el intervalo";
end

w = enIntervalo(1, 10, 11);
unless w then
	puts "Fuera del intervalo";
end