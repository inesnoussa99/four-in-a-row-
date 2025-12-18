#!/bin/bash

GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "======================================"
echo "  Tests Puissance 4 - Suite complète"
echo "======================================"

tests=(
test_game.pl
test_engine.pl
test_quit.pl
)

i=1
for t in "${tests[@]}"; do
    echo -e "${BLUE}[$i] Running $t...${NC}"
    swipl -q -s "$t" -g "run_tests, halt"
    echo -e "${GREEN}   ✓ $t OK ${NC}"
    echo ""
    ((i++))
done

echo -e "${GREEN}======================================"
echo -e "  ✓ Tous les tests terminés avec succès 🎉"
echo -e "======================================${NC}"
