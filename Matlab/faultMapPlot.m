% This script cumputes a bidimensional gaussian distribution and plots it.
% This probability function is assumed to represent the probability of a fault
% to happen on the chip in the coordinates (x1, x2).
% After that, a map is created where random faults are simulated. The value
% 0 in the map means 'no fault' and 1 means 'fault', whereas -1 indicates 
% the points out of the circular wafer.

clear all
close all
clc

% Initializing variables for gussian plot
ray = 30;
maximumFaultProbability = 0.5;

x1 = 0:1:2*ray;
x2 = x1;

mean = [ray,ray];
normalizedVariance = [1, 0; 0, 5];
variance = ray.*normalizedVariance;

[X1,X2] = meshgrid(x1,x2);

rng = 'default'; % for reproducibility

% Computing and plotting multivariate normal probability density function
Z = mvnpdf([X1(:), X2(:)], mean, variance);
Z = reshape(Z, length(x1), length(x2));

figure(1)
surf(X1,X2,Z)
title('Probability density function')

% Filling sqare matrix with simbolic values. Fault are deloyed according to
% the probabilit function.
faultMap = fillSquareGrid(Z, 2*ray, maximumFaultProbability);
faultMap = createCircularGrid(faultMap, ray);
faultNumber = length(find(faultMap==1));

% Plotting fault map
figure(2)
pcolor(faultMap)
title('Fault map')

% KDE
% call the routine, which has been saved in the current directory
[j, i] = find(faultMap==1); % find seachs elements by columns
[bandwidth,density,X,Y]=kde2d([i,j], 64);
% plot the data and the density estimate
figure(3)
hold on
contour3(X,Y,density,10); 
plot(i,j,'r.','MarkerSize',5);
title('Extimated density contours')
figure(4)
surf(X,Y,density);
title('Extimated function')

figure(5)
subplot(2,2,1)
surf(X1,X2,Z)
xlabel('x');
ylabel('y');
title('Probability density function')
subplot(2,2,2)
pcolor(faultMap)
xlabel('x');
ylabel('y');
title('Fault map')
subplot(2,2,3)
hold on
contour3(X,Y,density,10); 
plot(i,j,'r.','MarkerSize',5)
xlim([0, 2*ray]);
ylim([0, 2*ray]);
xlabel('x');
ylabel('y');
title('Extimated density contours')
subplot(2,2,4)
surf(X,Y,density);
xlim([0, 2*ray]);
ylim([0, 2*ray]);
xlabel('x');
ylabel('y');
title('Extimated function')
