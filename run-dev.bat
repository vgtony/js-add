@echo off
REM Quick development launcher for JavaScript Calculator
REM This script builds and runs the calculator in one command

title JS Calculator - Development Build
color 0A

echo 🔧 Building JavaScript Calculator...
cd /d "%~dp0"
stack build

if %errorlevel% neq 0 (
    echo ❌ Build failed! Check for compilation errors.
    echo Press any key to exit...
    pause >nul
    exit /b 1
)

echo ✅ Build successful!
echo 🚀 Starting calculator...
echo.
stack exec js-add

echo.
echo 👋 Calculator closed.
pause