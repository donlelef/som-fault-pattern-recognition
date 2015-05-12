function [errorMatrix, errorEvaluationNumber] = computeErrorInCircularGrid(extimatedFunction, trueFunction, x, y, ray)
% Find the mean square error between two matrix considering only the 
% value in a circular grid with ray "ray". It is assumed that both
% the real function and the extimated one are computed on the same x and y
% values.

errorEvaluationNumber=0;
for i=1:length(x)
    for j=1:length(y)
        if((x(i)-ray)^2+(y(j)-ray)^2<ray^2)
            errorMatrix(i,j)=((extimatedFunction(i,j)-trueFunction(i,j))^2)/trueFunction(i,j);
            errorEvaluationNumber = errorEvaluationNumber+1;
        else
            errorMatrix(i,j)=0;
        end
    end
end
end

