set -e

make=$(which make)

stack=$(which stack)

git=$(which git)

if [ -z $make ]; then
    echo "Please install GNU Make to run the installer"
    exit 1
fi

if [ -z $stack ]; then
    echo "Please install the stack build system for Haskell to run the installer"
    exit 1
fi

if [ -z $git ]; then
    echo "You must have git installed"
    exit 1
fi

echo "Using Git at $git"
echo "Using Stack at $stack"
echo "Using Make at $make"

rm -rf /tmp/glados

$git clone "https://github.com/TopineurInc/Glados.git" /tmp/glados

make -C /tmp/glados

printf "\nYou may enter your password to run the following command and add the glados to your path."
echo "sudo mv /tmp/glados/glados /usr/local/bin"
sudo mv /tmp/glados/glados /usr/local/bin

rm -rf /tmp/glados
