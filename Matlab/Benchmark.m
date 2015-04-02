% This script cumputes a bidimensional gaussian distribution and plots it.
% This probability function is assumed to represent the probability of a fault
% to happen on the chip in the coordinates (x1, x2).
% After that, a map is created where random faults are simulated. The value
% 0 in the map means 'no fault' and 1 means 'fault', whereas -1 indicates 
% the points out of the circular wafer.

clear all
close all
clc

% Initializing benchmark parameters
ray = 30;
maximimumFaultProbabilities = 0.1:0.01:1;
testPerPattern = 5;
patterns = 1; % TO BE IMPROVED other pattern needed
testNumber = testPerPattern * patterns;

for h = 1:length(maximimumFaultProbabilities);
    maximumFaultProbability = maximimumFaultProbabilities(h);
    totalError = 0;
    for k = 1:testNumber
        % Computing multivariate normal probability density function
        varX = 1;
        varY = 1;
        Z = GaussianDensity(ray, varX, varY);
        
        % Filling sqare matrix with simbolic values. Fault are deloyed according to
        % the probabilit function.
        faultMap = fillSquareGrid(Z, 2*ray, maximumFaultProbability);
        faultMap = createCircularGrid(faultMap, ray);
        faultNumber = length(find(faultMap==1));
        
        % KDE
        % call the routine, which has been saved in the current directory
        [j, i] = find(faultMap==1); % find seach elements by columns
        [bandwidth,extimatedDensity,X,Y] = kde2d([i,j], 64);
        
        % BENCHMARK
        % Computing and saving the average square error, ie the difference
        % between the value of the extimeted function and the value of the real
        % one.
        mean = [ray, ray]; % TO BE FIXED: duplicate code
        sigma = [varX, 0; 0, varY];
        trueDensity = mvnpdf([X(:), Y(:)], mean, sigma);
        trueDensity = reshape(trueDensity, length(X), length(Y));
        errorMatrix = (trueDensity - extimatedDensity).^2;
        error = sum(sum(errorMatrix));
        totalError = totalError + error;
    end
    meanError(h) = totalError/testNumber;
end
figure(1)
plot(maximimumFaultProbabilities, meanError)
title('Error vs maximumFaultProbability')
