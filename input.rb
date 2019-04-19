declare a, b;
b = "ne";
a = [[-1,1],b,3,4,5,6];

for x in ((-1..10) step 2) do
	x;
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
	#puts "En el intervalo";
	w;
end

w = enIntervalo(1, 10, 11);
unless w then
	#puts "Fuera del intervalo";
	w;
end


def fact(n)        
	declare to_return; 
	if(n == 0 ) then             
		to_return = 1;         
	else                          
		to_return = (n * (fact((n - 1))));
	end     
	return to_return; 
end    
puts "el factorial de 5 es",    (fact(5));

  
