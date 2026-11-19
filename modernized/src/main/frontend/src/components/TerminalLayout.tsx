import React, { useEffect } from 'react';
import '../styles/IBM3270.css';

interface TerminalLayoutProps {
  title: string;
  tranId: string;
  onPfKey?: (key: string) => void;
  children: React.ReactNode;
}

export const TerminalLayout: React.FC<TerminalLayoutProps> = ({
  title,
  tranId,
  onPfKey,
  children,
}) => {
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'F1' || e.key === 'F3' || e.key === 'F7' || e.key === 'F8' || e.key === 'Enter') {
        e.preventDefault();
        onPfKey?.(e.key.toUpperCase());
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [onPfKey]);

  return (
    <div className="ibm-terminal-screen">
      <div className="terminal-header">
        <span>TRAN: {tranId}</span>
        <span>CARDDEMO - {title.toUpperCase()}</span>
        <span>DATE: {new Date().toLocaleDateString()}</span>
      </div>
      <div className="terminal-body">{children}</div>
      <div className="terminal-footer">
        <button className="pf-key-badge" onClick={() => onPfKey?.('ENTER')}>Enter: Submit</button>
        <button className="pf-key-badge" onClick={() => onPfKey?.('F1')}>F1: Help</button>
        <button className="pf-key-badge" onClick={() => onPfKey?.('F3')}>F3: Exit</button>
        <button className="pf-key-badge" onClick={() => onPfKey?.('F7')}>F7: Page Up</button>
        <button className="pf-key-badge" onClick={() => onPfKey?.('F8')}>F8: Page Down</button>
      </div>
    </div>
  );
};
