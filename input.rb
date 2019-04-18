declare a;
a = [1,2,3,4,5,6];

puts (a[1,3]);


for x in (1..10) do
	puts x;
end

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
