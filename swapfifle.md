#### 1. **检查是否已有活动的交换文件**
运行以下命令，检查当前是否存在正在使用的交换文件：
```bash
sudo swapon --show
```
如果输出中显示已有 `/swapfile` 或其他交换文件正在使用，您需要先禁用现有的交换文件。

#### 2. **禁用当前交换文件（如果有）**
若 `/swapfile` 显示在上一步的结果中，请先禁用它：
```bash
sudo swapoff /swapfile
```
禁用后，重新尝试创建交换文件。

#### 3. **删除旧的交换文件（可选）**
如果确定不需要当前的 `/swapfile`，可以删除旧文件：
```bash
sudo rm /swapfile
```

#### 4. **重新创建交换文件**
使用 `fallocate` 创建新的 128G 交换文件：
```bash
sudo fallocate -l 128G /swapfile
```
如果 `fallocate` 仍然失败，可以尝试 `dd` 命令：
```bash
sudo dd if=/dev/zero of=/swapfile bs=1G count=128
```
> 这个命令通过逐块写入零字节来创建交换文件，速度可能比 `fallocate` 慢，但兼容性更好。

#### 5. **设置正确的文件权限**
为安全起见，交换文件需要有合适的权限：
```bash
sudo chmod 600 /swapfile
```

#### 6. **将文件设置为交换空间**
```bash
sudo mkswap /swapfile
```

#### 7. **启用交换文件**
```bash
sudo swapon /swapfile
```

#### 8. **验证是否成功启用**
再次检查是否启用了新的交换文件：
```bash
sudo swapon --show
```

#### 9. **永久生效（更新 `/etc/fstab`）**
编辑 `/etc/fstab` 文件以确保系统重启后自动启用交换文件：
```bash
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
```

---

### **注意事项**
- 确保磁盘有足够空间创建 128G 的交换文件。
- 如果系统内存较小，建议避免分配过大的交换文件，以免引发性能问题。

完成后，您的系统应该会成功启用 128G 的交换空间！