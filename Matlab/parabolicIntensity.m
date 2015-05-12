function intensity = parabolicIntensity(ray, coefficient)
% PARABOLICINITENSITY returns a paraboloid with a given whithin a square of 
% side 2ray*2ray and center [ray, ray]. The coefficient of (x^2 + y^2) has 
% to be specified.

% Initalizing parameters
x = 1:1:2*ray;
y = x;
[X, Y] = meshgrid(x,y);

% Computing function
intensity = coefficient.*((X-ray).^2 + (Y-ray).^2);

end

