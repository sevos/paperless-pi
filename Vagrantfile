# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  config.vm.box = "base"

  #config.vm.box_url = "https://dl.dropbox.com/u/14292474/vagrantboxes/precise64-ruby-1.9.3-p194.box"


  config.vm.boot_mode = :gui

  # config.vm.network :hostonly, "192.168.33.10"

  # config.vm.network :bridged

  config.vm.forward_port 8080, 8080

  # config.vm.share_folder "v-data", "/vagrant_data", "../data"

end
