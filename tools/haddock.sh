set -e

export LANG="C.UTF-8"

cabal haddock --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
fd -t f . docs | xargs -I % sh -c "rm -r %"

dir=$(fd -t d html dist-newstyle/build)

cp -r $dir/relative-time/* docs/

