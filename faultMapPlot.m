clear all
close all
clc

x = 0:.1:10;
y = x;

ray = length(x)/2;

mediaX = 5;
mediaY = 5;
sigmaX = 4;
sigmaY = 2;

[X,Y] = meshgrid(x,y);

Z = (1/(2*pi*sigmaX*sigmaY))* exp(-(1/2) * ( (((X-mediaX).^2)/sigmaX) + (((Y-mediaY).^2)/sigmaY) ));

figure(1)
surf(X,Y,Z)

maxZ = max(max(Z));

for (i=1:length(x))
    for (j=1:length(y))
        
        if ((rand*(maxZ*10) < Z(i,j)) && ((i-ray)^2+(j-ray)^2 <= ray^2))
            faultMap(i,j) = 1;
        else
            if ((i-ray)^2+(j-ray)^2 > ray^2)
                faultMap(i,j) = -1;
            else
            faultMap(i,j) = 0;
            end
        end
    end
end

faultNumber = length(find(faultMap));

figure(2)
pcolor(faultMap)

