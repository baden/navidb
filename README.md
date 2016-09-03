# navidb [![Build Status](https://travis-ci.org/baden/navidb.png)](https://travis-ci.org/baden/navidb)

Database component for  [navicc](https://github.com/baden/navicc) project.

Erlang versions supported: 17.1 and up

## Dependencies

### Erlang

```shell
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
rm ./erlang-solutions_1.0_all.deb
sudo apt-get update
sudo apt-get install -y erlang
```

### MongoDB

```shell
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
echo "deb http://repo.mongodb.org/apt/ubuntu `lsb_release -cs`/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
sudo apt-get update
sudo apt-get install -y mongodb-org
sudo servise mongod start
```

## Build the library

```shell
make
```

## Testing

```shell
make elvis
make tests
# make xref
```

## Documentation

```shell
make docs
```

Documentation for the library can be obtained at http://baden.github.io/navidb/
