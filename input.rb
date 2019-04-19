

def enIntervalo(min,max,value)
	if (min < value) then
		if (value < max) then
			return true;
		end
	else
		return false;
	end
end

declare w;

w = enIntervalo(1, 10, 5);
if w then
	puts "En el intervalo";
end

w = enIntervalo(1, 10, 11);
unless w then
	puts "Fuera del intervalo";
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
puts "el factorial de 5 es", (fact(5));
  
