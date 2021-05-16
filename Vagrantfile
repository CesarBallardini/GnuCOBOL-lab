# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

HOSTNAME = "coboldev"

$msg = <<MSG
------------------------------------------------------
GNU COBOL

  cobc --version  # muestra la version de GNU COBOL instalada
  dialectos=" ibm cobol85 rm ibm-strict mvs-strict cobol2002 default cobol2014 xopen mf-strict mf bs2000-strict bs2000 acu-strict acu rm-strict mvs "
                       
Binarios:
 - /usr/local/bin/cobc:    compilador
 - /usr/local/bin/cobcrun: runtime

Compilar y generar ejecutable
  cobc -x /vagrant/hello/hello.cbl
  ./hello

Compilar módulos dinámicos
  cobc -m /vagrant/hello/hello.cbl
  cobcrun hello

COBOL scripts
   /vagrant/hello/hello.sh

IDE:
  opencobolide: la mejor, aunque ya esta sin mantenimiento
  subl: SublimeText3 con el plugin para COBOL

------------------------------------------------------
MSG

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  if Vagrant.has_plugin?("vagrant-hostmanager")
    config.hostmanager.enabled = true
    config.hostmanager.manage_host = true
    config.hostmanager.manage_guest = true
    config.hostmanager.ignore_private_ip = false
    config.hostmanager.include_offline = true

    # uso cachier con NFS solamente si el hostmanager gestiona los nombres en /etc/hosts del host
    if Vagrant.has_plugin?("vagrant-cachier")
      # Configure cached packages to be shared between instances of the same base box.
      # More info on http://fgrehm.viewdocs.io/vagrant-cachier/usage
      config.cache.scope = :box

      # OPTIONAL: If you are using VirtualBox, you might want to use that to enable
      # NFS for shared folders. This is also very useful for vagrant-libvirt if you
      # want bi-directional sync

      # NOTA: con encrypted HOME, no funciona el NFS, si no es tu caso, descomenta los parametros siguientes:
      #config.cache.synced_folder_opts = {
      #  type: :nfs,
      #  # The nolock option can be useful for an NFSv3 client that wants to avoid the
      #  # NLM sideband protocol. Without this option, apt-get might hang if it tries
      #  # to lock files needed for /var/cache/* operations. All of this can be avoided
      #  # by using NFSv4 everywhere. Please note that the tcp option is not the default.
      #  mount_options: ['rw', 'vers=3', 'tcp', 'nolock', 'fsc' , 'actimeo=2']
      #}

      # For more information please check http://docs.vagrantup.com/v2/synced-folders/basic_usage.html
   end

  end

 config.vm.post_up_message = $msg

 config.vm.define HOSTNAME do |srv|

    #srv.vm.box = "ubuntu/focal64"
    srv.vm.box = "ubuntu/bionic64"
    #srv.vm.box = "ubuntu/xenial64"
    #srv.vm.box = "ubuntu/xenial32"

    srv.vm.network "private_network", ip: "192.168.33.10"
    srv.vm.box_check_update = false
    srv.ssh.forward_agent = true
    srv.ssh.forward_x11 = true
    srv.vm.hostname = HOSTNAME

    if Vagrant.has_plugin?("vagrant-hostmanager")
      srv.hostmanager.aliases = %W(#{HOSTNAME+".virtual.ballardini.com.ar"} #{HOSTNAME} )
    end

    srv.vm.provider :virtualbox do |vb|
      vb.gui = false
      vb.cpus = 1
      vb.memory = "512"

      # https://www.virtualbox.org/manual/ch08.html#vboxmanage-modifyvm mas parametros para personalizar en VB
    end
  end

    ##
    # Aprovisionamiento
    #
    config.vm.provision "fix-no-tty", type: "shell" do |s|  # http://foo-o-rama.com/vagrant--stdin-is-not-a-tty--fix.html
        s.privileged = false
        s.inline = "sudo sed -i '/tty/!s/mesg n/tty -s \\&\\& mesg n/' /root/.profile"
    end
    config.vm.provision "ssh_pub_key", type: :shell do |s|
      begin
          ssh_pub_key = File.readlines("#{Dir.home}/.ssh/id_rsa.pub").first.strip
          s.inline = <<-SHELL
            mkdir -p /root/.ssh/
            touch /root/.ssh/authorized_keys
            echo #{ssh_pub_key} >> /home/vagrant/.ssh/authorized_keys
            echo #{ssh_pub_key} >> /root/.ssh/authorized_keys
          SHELL
      rescue
          puts "No hay claves publicas en el HOME de su pc"
          s.inline = "echo OK sin claves publicas"
      end
    end

    config.vm.provision "actualiza", type: "shell" do |s|
        s.privileged = false
        s.inline = <<-SHELL
          export DEBIAN_FRONTEND=noninteractive
          export APT_LISTCHANGES_FRONTEND=none
          export APT_OPTIONS=' -y --allow-downgrades --allow-remove-essential --allow-change-held-packages -o Dpkg::Options::=--force-confdef -o Dpkg::Options::=--force-confold '

          sudo -E apt-get --purge remove apt-listchanges -y > /dev/null 2>&1
          sudo -E apt-get update -y -qq > /dev/null 2>&1
          sudo dpkg-reconfigure --frontend=noninteractive libc6 > /dev/null 2>&1
          [ $( lsb_release -is ) != "Debian" ] && sudo -E apt-get install linux-image-generic ${APT_OPTIONS}
          sudo -E apt-get upgrade ${APT_OPTIONS} > /dev/null 2>&1
          sudo -E apt-get dist-upgrade ${APT_OPTIONS} > /dev/null 2>&1
          sudo -E apt-get autoremove -y > /dev/null 2>&1
          sudo -E apt-get autoclean -y > /dev/null 2>&1
          sudo -E apt-get clean > /dev/null 2>&1
        SHELL
    end

    config.vm.provision "ansible-provision", type: :ansible do |ansible|
      ansible.playbook = "provision/ansible/playbook.yml"
      ansible.verbose = "vv"
      ansible.become= false
    end


    config.vm.provision "instala_opencobolide", type: "shell", privileged: false, path: "provision/instala-opencobol-ide.sh"
    config.vm.provision "instala_sublime", type: "shell", privileged: false, path: "provision/instala-sublime-text-3.sh"
    config.vm.provision "instala_notepad-plus-plus", type: "shell", privileged: false, path: "provision/instala-notepad-plus-plus.sh"

#    config.vm.provision "ansible-test", type: :ansible do |ansible|
#      ansible.playbook = "provision/ansible/test.yml"
#      ansible.become= false
#    end

end

