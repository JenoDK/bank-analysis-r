package com.jeno;

import com.jeno.client.JFilePicker;
import com.jeno.rserve.RserveParser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;

public class Main extends JFrame {

    public Main() {
        super("Bank analytics");

        setLayout(new FlowLayout());

        // set up a file picker component
        JFilePicker filePicker = new JFilePicker("Pick a file", "Browse...");
        filePicker.setMode(JFilePicker.MODE_SAVE);
        filePicker.addFileTypeFilter(".csv", "CSV files");

        // access JFileChooser class directly
        JFileChooser fileChooser = filePicker.getFileChooser();
        fileChooser.setCurrentDirectory(new File("D:/"));

        // add the component to the frame
        add(filePicker);

        JButton generateButton = new JButton("Generate");
        generateButton.addActionListener(ignored -> add(RserveParser.generateReport(fileChooser.getSelectedFile().getAbsolutePath())));

        add(generateButton);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(600, 130);
        setResizable(false);
        setLocationRelativeTo(null);    // center on screen
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> new Main().setVisible(true));
    }

}
