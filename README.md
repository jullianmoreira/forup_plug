# 🖨️ **Sistema de Integração e Controle de Impressão**

Bem-vindo ao **Sistema de Integração e Controle de Impressão**, uma solução moderna e eficiente para gerenciar e personalizar impressões em diferentes ambientes. 🚀

---

## 🌟 **Visão Geral**

Este projeto foi desenvolvido para atender às necessidades de integração e controle de impressão, oferecendo flexibilidade e robustez.  
**Dividido em duas partes principais**, ele combina a eficiência de um serviço online com a personalização e controle de um serviço local.

---

## 🔗 **Partes do Sistema**

### **1️⃣ Serviço Online de Integração**
- Gerencia a comunicação com sistemas externos via APIs.
- Responsável por centralizar as solicitações de impressão.
- Alimentado por um banco de dados online **PostgreSQL** para maior desempenho e escalabilidade.
- Foco na **disponibilidade 24/7** e alta segurança.

### **2️⃣ Serviço Local de Controle**
- Atua diretamente nos dispositivos de impressão.
- Permite personalização detalhada dos documentos antes da impressão.
- Conexão com banco de dados local **SQLite**, garantindo operações rápidas e independência em situações offline.
- Comunicação direta com o **ERP**, utilizando o banco de dados **MongoDB**.

---

## 🛠️ **Principais Tecnologias**
- **PostgreSQL**: Banco de dados online para integração segura e confiável.
- **MongoDB**: Utilizado pelo ERP para garantir alta performance em consultas.
- **SQLite**: Banco de dados leve para operações locais rápidas e confiáveis.
- **RESTful APIs**: Para comunicação entre os serviços online e locais.

---

## 🚀 **Funcionalidades**
- **Impressão Integrada**: Solicitações de impressão sincronizadas com o ERP.
- **Personalização Avançada**: Ajuste de layout, formatação e outros parâmetros antes da impressão.
- **Monitoramento em Tempo Real**: Controle sobre todas as solicitações e status de impressão.
- **Compatibilidade Universal**: Suporte para diversas marcas e modelos de impressoras.

---

## 🏗️ **Arquitetura**
```plaintext
[Usuário/ERP] 
     ↓ (API REST)
[Serviço Online - PostgreSQL]
     ↓ (Sincronização)
[Serviço Local - SQLite & MongoDB]
     ↓
[Dispositivo de Impressão]

📦 forup_plug
├── 📁 forup_client_plug      # Módulo cliente do sistema
├── 📁 forup_online_plug      # Serviço de integração online
├── 📁 release                # Arquivos para distribuição
├── 📄 .gitattributes         # Configurações de atributos Git
├── 📄 .gitignore             # Arquivos ignorados pelo Git
├── 📄 LICENSE                # Licença do projeto
└── 📄 README.md              # Documentação do projeto
