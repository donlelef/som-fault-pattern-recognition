% KED test with univariate graussian function

% Initializing workspace 
clc
clear all
close all


randn('seed',8192);
x = [randn(50,1); randn(50,1)+3.5];
[h, fhat, xgrid] = kde(x, 401);
figure;
hold on;
plot(xgrid, fhat, 'linewidth', 2, 'color', 'black');
plot(x, zeros(100,1), 'b+');
xlabel('x')
ylabel('Density function')
hold off;

