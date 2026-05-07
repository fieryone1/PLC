param()

# Simple PowerShell test runner for t6..t10
$root = Split-Path -Parent $MyInvocation.MyCommand.Definition
$expectedDir = Join-Path $root "tests\expected"
$allOk = $true

function Normalize-Text([string]$text) {
    if ($null -eq $text) { return "" }
    return (($text -replace "`r`n", "`n") -replace "`r", "").TrimEnd("`n")
}

for ($n = 6; $n -le 10; $n++) {
    $rql = "t$n.rql"
    Write-Host "Running test t$n..."
    $procOut = & stack exec -- plc-project-exe $rql 2>&1
    if ($procOut -eq $null) {
        $procOutText = ""
    } else {
        $procOutText = $procOut -join "`n"
    }
    $actual = Normalize-Text $procOutText

    $expectedPath = Join-Path $expectedDir ("t$n.out")
    if (Test-Path $expectedPath) {
        $expected = Normalize-Text (Get-Content -Raw $expectedPath)
    } else {
        $expected = ""
    }

    if ($actual -eq $expected) {
        Write-Host ("t{0}: PASS" -f $n)
    } else {
        Write-Host ("t{0}: FAIL" -f $n)
        Write-Host "--- Expected ---"
        Write-Host $expected
        Write-Host "--- Actual ---"
        Write-Host $actual
        $allOk = $false
    }
}

if ($allOk) { Write-Host "All tests passed."; exit 0 } else { Write-Host "Some tests failed."; exit 1 }
